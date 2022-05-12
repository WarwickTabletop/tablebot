{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Tablebot.Utility.SmartParser
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Tablebot.Utility.SmartParser where

import Control.Monad.Exception (MonadException (catch))
import Data.Default (Default (def))
import Data.Proxy (Proxy (..))
import Data.Scientific
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Discord.Interactions
import Discord.Types
import GHC.OldList (find)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Tablebot.Internal.Handler.Command (parseValue)
import Tablebot.Utility.Discord (getChannel, interactionResponseComponentsUpdateMessage, interactionResponseCustomMessage, sendCustomMessage)
import Tablebot.Utility.Exception (BotException (InteractionException, ParserException), catchBot, embedError, throwBot)
import Tablebot.Utility.Parser
import Tablebot.Utility.Types
import Text.Megaparsec (MonadParsec (eof, try), chunk, many, observing, optional, (<?>), (<|>))

-- | The type class representing some data we can extract data from.
-- Needed for things like getting a GuildMember, message id, guild id.
--
-- Only defined for Message and Interaction.
class Context a where
  contextUserId :: a -> ParseUserId
  contextGuildId :: a -> EnvDatabaseDiscord s (Maybe GuildId)
  contextMember :: a -> Maybe GuildMember
  contextMessageId :: a -> Maybe MessageId

instance Context Message where
  contextUserId = ParseUserId . userId . messageAuthor
  contextGuildId m = case messageGuildId m of
    Just a -> pure $ Just a
    Nothing -> do
      let chanId = messageChannelId m
      channel <- getChannel chanId
      case fmap channelGuild channel of
        Right a -> pure $ Just a
        Left _ -> pure Nothing
  contextMember = messageMember
  contextMessageId = return . messageId

instance Context Interaction where
  -- this is safe to do because we are guaranteed to get either a user or a member
  contextUserId i = ParseUserId $ maybe 0 userId (either memberUser Just mor)
    where
      (MemberOrUser mor) = interactionUser i
  contextGuildId i = return $ interactionGuildId i
  contextMember i = case interactionUser i of
    (MemberOrUser (Left m)) -> return m
    (MemberOrUser (Right _)) -> Nothing
  contextMessageId InteractionComponent {interactionMessage = m} = return $ messageId m
  contextMessageId InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataMessage {..}} = return applicationCommandDataTargetMessageId
  contextMessageId _ = Nothing

-- | Custom infix operator to replace the error of a failing parser (regardless
-- of parser position) with a user given error message.
--
-- Has some effects on other error parsing. Use if you want the error you give
-- to be the one that is reported (unless this is used at a higher level.)
--
-- Overwrites/overpowers WithError errors.
(<??>) :: Parser a -> String -> Parser a
(<??>) p s = do
  r <- observing p
  case r of
    Left _ -> fail s
    Right a -> return a

-- | @PComm@ defines function types that we can automatically turn into parsers
-- by composing a parser per input of the function provided.
-- For example, @Int -> Maybe Text -> Message -> DatabaseDiscord s ()@ builds a
-- parser that reads in an @Int@, then some optional @Text@, and then uses
-- those to run the provided function with the arguments parsed and the message
-- itself.
--
-- The arguments to this class are the type of the function, the type of the
-- environment, the type of the context (either Message or Interaction), and the
-- type of the result of the function (which is either () or MessageDetails
-- usually).
class PComm commandty s context returns where
  parseComm :: (Context context) => commandty -> Parser (context -> EnvDatabaseDiscord s returns)

-- TODO: verify that all the parsers for PComm actually work

-- If there is the general case where we have just what we want to parse, then
-- return it
-- (1)
instance {-# OVERLAPPING #-} PComm (t -> EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> return comm

-- If we have the specific case where we are returning `()`, parse eof as well.
-- This should cover the base case for the rest of the program that doesn't use
-- more complex stuff.
-- (2)
instance {-# OVERLAPPING #-} PComm (t -> EnvDatabaseDiscord s ()) s t () where
  parseComm comm = skipSpace >> eof >> return comm

-- If an action takes a message and returns a message details and we want it to
-- return unit, assume that it wants to be sent, and send it. eof this as well
-- (3)
instance {-# OVERLAPPING #-} PComm (Message -> EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm m >>= sendCustomMessage m)

-- When there is no context to the function (eg no Message or Interaction),
-- just run the action. don't parse eof cause we may wanna return.
-- similar to (1)
-- (4)
instance PComm (EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> return (const comm)

-- When there is no context to the function (eg no Message or Interaction),
-- just run the action. effectively the function hasn't interacted with the `t`.
-- parse eof because we have unit here. similar to (2)
-- (5)
instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s ()) s t () where
  parseComm comm = skipSpace >> eof >> return (const comm)

-- if we're in a message context and have a message details but want to return
-- unit, assume that we want to send it, and send it. similar to (3)
-- (6)
instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm >>= sendCustomMessage m)

-- Recursive case is to parse the domain of the function type, then the rest.
-- (7)
instance {-# OVERLAPPABLE #-} (CanParse a, PComm as s t r) => PComm (a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (comm this)

-- if we have two contexts for some reason, collapse them if the resultant can
-- be parsed
-- (8)
instance {-# OVERLAPPABLE #-} (PComm (t -> as) s t r) => PComm (t -> t -> as) s t r where
  parseComm comm = parseComm (\m -> comm m m)

-- if we have a context and then some parseable value, effectively juggle the
-- context so that parsing continues (and the context is passed on)
-- (9)
instance {-# OVERLAPPABLE #-} (Context t, CanParse a, PComm (t -> as) s t r) => PComm (t -> a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (`comm` this)

-- special value case - if we get ParseUserId, we need to get the value from
-- the context. so, get the value from the context, and then continue parsing.
-- (10)
instance {-# OVERLAPPABLE #-} (PComm (t -> as) s t r) => PComm (ParseUserId -> as) s t r where
  parseComm comm = parseComm $ \(m :: t) -> comm (contextUserId m)

-- | @CanParse@ defines types from which we can generate parsers.
class CanParse a where
  pars :: Parser a
  parsThenMoveToNext :: Parser a
  parsThenMoveToNext = pars <* (eof <|> skipSpace1)

-- Note: since FromString and (Read, Integral) can overlap, we cannot specify
-- this instance as FromString a => CanParse a.
instance CanParse Text where
  pars = pack <$> word

-- This overlaps CanParse [a], since String = [Char].
instance {-# OVERLAPPING #-} CanParse String where
  pars = word

-- | @Quoted a@ defines an input of type @a@ that is contained within quotes.
newtype Quoted a = Qu {quote :: a} deriving (Show)

instance IsString a => CanParse (Quoted a) where
  pars = Qu . fromString <$> quoted

instance (ParseShow a) => ParseShow (Quoted a) where
  parseShow (Qu a) = "\"" <> parseShow a <> "\""

-- A parser for @Maybe a@ attempts to parse @a@, returning @Just x@ if
-- correctly parsed, else @Nothing@.
instance CanParse a => CanParse (Maybe a) where
  pars = optional $ try (pars @a)

  -- Note: we override @parsThenMoveToNext@:
  -- there will be no spaces to parse if the argument isn't present.
  parsThenMoveToNext =
    pars >>= \case
      Nothing -> return Nothing
      Just val -> Just val <$ (eof <|> skipSpace1)

-- A parser for @[a]@ parses any number of @a@s.
instance {-# OVERLAPPABLE #-} CanParse a => CanParse [a] where
  pars = many pars

-- A parser for @Either a b@ attempts to parse @a@, and if that fails then
-- attempts to parse @b@.
instance (CanParse a, CanParse b) => CanParse (Either a b) where
  pars = (Left <$> try (pars @a)) <|> (Right <$> pars @b)

-- TODO: automate creation of tuple instances using TemplateHaskell
instance (CanParse a, CanParse b) => CanParse (a, b) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- pars @b
    return (x, y)

instance (CanParse a, CanParse b, CanParse c) => CanParse (a, b, c) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- pars @c
    return (x, y, z)

instance (CanParse a, CanParse b, CanParse c, CanParse d) => CanParse (a, b, c, d) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- parsThenMoveToNext @c
    w <- pars @d
    return (x, y, z, w)

instance (CanParse a, CanParse b, CanParse c, CanParse d, CanParse e) => CanParse (a, b, c, d, e) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- parsThenMoveToNext @c
    w <- parsThenMoveToNext @d
    v <- pars @e
    return (x, y, z, w, v)

-- | @Exactly s@ defines an input exactly matching @s@ and nothing else.
data Exactly (s :: Symbol) = Ex

instance KnownSymbol s => CanParse (Exactly s) where
  pars = chunk (pack $ symbolVal (Proxy :: Proxy s)) >> return Ex

-- | @WithError err x@ parses an @x@, reporting @err@ if the parsing of @x@
-- fails.
newtype WithError (err :: Symbol) x = WErr x

instance (KnownSymbol err, CanParse x) => CanParse (WithError err x) where
  pars = (WErr <$> try (pars @x)) <?> symbolVal (Proxy :: Proxy err)

-- | Parsing implementation for all integral types
-- Overlappable due to the really flexible head state
instance {-# OVERLAPPABLE #-} (Integral a, Read a) => CanParse a where
  pars = integer

instance CanParse Double where
  pars = double

instance CanParse () where
  pars = eof

instance CanParse Snowflake where
  pars = Snowflake . fromInteger <$> posInteger

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput a = ROI {unROI :: a}

instance IsString a => CanParse (RestOfInput a) where
  pars = ROI . fromString <$> untilEnd

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput1 a = ROI1 a

instance IsString a => CanParse (RestOfInput1 a) where
  pars = ROI1 . fromString <$> untilEnd1

-- | Data type to represent parsing a user id from the context.
newtype ParseUserId = ParseUserId {parseUserId :: UserId}

-- | Labelled value for use with smart commands.
--
-- This is for use with slash commands, where there is a name and description
-- required.
newtype Labelled (name :: Symbol) (desc :: Symbol) a = Labelled {unLabel :: a}

-- | Easily make a labelled value.
labelValue :: forall n d a. a -> Labelled n d a
labelValue = Labelled @n @d

-- | Get the name and description of a labelled value.
getLabelValues :: forall n d a. (KnownSymbol n, KnownSymbol d) => Proxy (Labelled n d a) -> (Text, Text)
getLabelValues _ = (pack (symbolVal (Proxy :: Proxy n)), pack (symbolVal (Proxy :: Proxy d)))

-- | Parse a labelled value, by parsing the base value and adding the label
-- values.
instance (CanParse a) => CanParse (Labelled n d a) where
  pars = labelValue <$> pars

-- | @noArguments@ is a type-specific alias for @parseComm@ for commands that
-- have no arguments (thus making it extremely clear).
noArguments :: (Message -> EnvDatabaseDiscord d ()) -> Parser (Message -> EnvDatabaseDiscord d ())
noArguments = parseComm

--------------------------------------------------------------------------------
-- Interactions stuff
----

-- | Creates both the slash command creation data structure and the parser for
-- the command, and creates the EnvApplicationCommandRecv for the command by
-- combining them.
--
-- Takes the name and description for a slash command, and its function.
makeApplicationCommandPair :: forall t s. (MakeAppComm t, ProcessAppComm t s) => Text -> Text -> t -> Maybe (EnvApplicationCommandRecv s)
makeApplicationCommandPair name desc f = do
  cac <- makeSlashCommand name desc (Proxy :: Proxy t)
  return $ ApplicationCommandRecv cac (processAppComm f)

-- | Make the creation data structure for a slash command when given a proxy for
-- a function's type.
makeSlashCommand :: (MakeAppComm t) => Text -> Text -> Proxy t -> Maybe CreateApplicationCommand
makeSlashCommand name desc p =
  createChatInput name desc >>= \cac ->
    return $
      cac
        { createOptions = Just $ OptionsValues $ makeAppComm p
        }

-- | Create a series of command option values from the given types.
--
-- This is making the arguments for a text input/slash command from
-- a proxy of the given function.
class MakeAppComm commandty where
  makeAppComm :: Proxy commandty -> [OptionValue]

-- As a base case, no more arguments
instance {-# OVERLAPPING #-} MakeAppComm (EnvDatabaseDiscord s MessageDetails) where
  makeAppComm _ = []

-- If there is a way to get an argument from a `ty`, then get that arg and continue recursion.
instance {-# OVERLAPPABLE #-} (MakeAppComm mac, MakeAppCommArg ty) => MakeAppComm (ty -> mac) where
  makeAppComm _ = makeAppCommArg (Proxy :: Proxy ty) : makeAppComm (Proxy :: Proxy mac)

instance {-# OVERLAPPABLE #-} (MakeAppComm mac) => MakeAppComm (ParseUserId -> mac) where
  makeAppComm _ = makeAppComm (Proxy :: Proxy mac)

-- | From a single value, make an argument for a slash command command.
class MakeAppCommArg commandty where
  makeAppCommArg :: Proxy commandty -> OptionValue

-- Create a labelled text argument. By default it is required and does not
-- have autocompeletion.
instance (KnownSymbol name, KnownSymbol desc) => MakeAppCommArg (Labelled name desc Text) where
  makeAppCommArg l = OptionValueString n d True (Left False)
    where
      (n, d) = getLabelValues l

-- Create a labelled argument that is optional.
instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Maybe t)) where
  makeAppCommArg _ =
    (makeAppCommArg (Proxy :: Proxy (Labelled name desc t)))
      { optionValueRequired = False
      }

-- When quoted text is required, just fake it and get a sub layer.
instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Quoted t)) where
  makeAppCommArg _ = makeAppCommArg (Proxy :: Proxy (Labelled name desc t))

-- As a base case, send the message produced

-- | Process an application command when given a function/value.
class ProcessAppComm commandty s where
  processAppComm :: commandty -> Interaction -> EnvDatabaseDiscord s ()

-- When left with just a MessageDetails, just send the message as an
-- interaction response.
instance {-# OVERLAPPING #-} ProcessAppComm (EnvDatabaseDiscord s MessageDetails) s where
  processAppComm comm i = comm >>= interactionResponseCustomMessage i

-- If there is already an interaction in this function call, apply it and
-- recurse.
instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (Interaction -> pac) s where
  processAppComm comm i = processAppComm (comm i) i

-- This is the main recursion case.
--
-- If the argument is a ProcessAppCommArg, then parse it and recurse.
instance {-# OVERLAPPABLE #-} (ProcessAppCommArg ty s, ProcessAppComm pac s) => ProcessAppComm (ty -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataChatInput {optionsData = opts}} = do
    t <- processAppCommArg (getVs opts)
    processAppComm (comm t) i
    where
      getVs (Just (OptionsDataValues vs)) = vs
      getVs _ = []
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

-- one specific implementation case when we want to parse out a user id.
instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (ParseUserId -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {interactionUser = MemberOrUser u} =
    case getUser of
      Nothing -> throwBot $ InteractionException "could not process args to application command"
      Just uid -> processAppComm (comm (ParseUserId uid)) i
    where
      getUser = userId <$> either memberUser Just u
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

-- | Process an argument for an application command.
--
-- Given a type `t`, parse a value of that type from the given list of option
-- values.
class ProcessAppCommArg t s where
  processAppCommArg :: [OptionDataValue] -> EnvDatabaseDiscord s t

-- | Given a string, find the first option value with that name in the list,
-- returning Nothing if none is found.
getValue :: String -> [OptionDataValue] -> Maybe OptionDataValue
getValue t = find ((== pack t) . optionDataValueName)

-- | Tries to extract an integer from a given option value.
integerFromOptionValue :: OptionDataValue -> Maybe Integer
integerFromOptionValue OptionDataValueInteger {optionDataValueInteger = Right i} = Just i
integerFromOptionValue _ = Nothing

-- | Tries to extract a scientific number from a given option value.
scientificFromOptionValue :: OptionDataValue -> Maybe Scientific
scientificFromOptionValue OptionDataValueNumber {optionDataValueNumber = Right i} = Just i
scientificFromOptionValue _ = Nothing

-- | Tries to extract a string from a given option value.
stringFromOptionValue :: OptionDataValue -> Maybe Text
stringFromOptionValue OptionDataValueString {optionDataValueString = Right i} = Just i
stringFromOptionValue _ = Nothing

-- there are a number of missing slash command argument types missing here, which I've not added yet.

-- extract a string of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Text) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueString _ (Right t)) -> return $ labelValue t
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract an integer of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Integer) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueInteger _ (Right i)) -> return $ labelValue i
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract a scientific number of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Scientific) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueNumber _ (Right i)) -> return $ labelValue i
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract a quote of the given type from the arguments
instance (KnownSymbol name, KnownSymbol desc, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Quoted t)) s where
  processAppCommArg is = processAppCommArg @(Labelled name desc t) is >>= \(Labelled a) -> return (labelValue (Qu a))

-- extract an optional data type from the arguments
instance (KnownSymbol name, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Maybe t)) s where
  processAppCommArg is = do
    let result = processAppCommArg is :: EnvDatabaseDiscord s (Labelled name desc t)
    ( do
        (Labelled l) <- result
        return (labelValue (Just l))
      )
      `catchBot` const (return $ labelValue Nothing)

-- | Given a function that can be processed to create a parser, create an action
-- for it using the helper. Uses `parseComm` to generate the required parser.
--
-- Components use a unique string as their identifier. We can use this to
-- run the normal command parser on, hence the use of PComm.
--
-- If the boolean is False, a reply is sent to the interaction message. If the
-- boolean is True, the original message is updated.
--
-- For more information, check the helper `processComponentInteraction'`.
processComponentInteraction :: (PComm f s Interaction MessageDetails) => f -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction f = processComponentInteraction' (parseComm f)

-- | Given a parser that, when run, returns a function taking an interaction
-- and returns a database action on some MessageDetails, run the action.
--
-- If the boolean is true, the message the component is from is updated. Else,
-- a message is sent as the interaction response.
--
-- The format of the Text being given should be of space separated values,
-- similar to the command structure.
processComponentInteraction' :: Parser (Interaction -> EnvDatabaseDiscord s MessageDetails) -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction' compParser updateOriginal i@InteractionComponent {componentData = idc} = errorCatch $ do
  let componentSend
        | updateOriginal = interactionResponseComponentsUpdateMessage i
        | otherwise = interactionResponseCustomMessage i
  action <- parseValue (skipSpace *> compParser) (componentDataCustomId idc) >>= ($ i)
  componentSend action
  where
    catchParserException e@(ParserException _ _) = interactionResponseCustomMessage i $ (messageDetailsBasic "something (likely) went wrong when processing a component interaction") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    catchParserException e = interactionResponseCustomMessage i $ (messageDetailsBasic "") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    errorCatch = (`catch` catchParserException)
processComponentInteraction' _ _ _ = throwBot $ InteractionException "could not process component interaction"

-- | Function to only allow use of an interaction if the requestor matches
-- a Snowflake at the beginning of the input. This uses a helper, and by default
-- sends an ephermeral message with the text "You don't have permission to use
-- this component."
--
-- Helper is `onlyAllowRequestor'`.
onlyAllowRequestor :: forall f. (PComm f () Interaction MessageDetails) => f -> Parser (Interaction -> DatabaseDiscord MessageDetails)
onlyAllowRequestor =
  onlyAllowRequestor'
    ( (messageDetailsBasic "You don't have permission to use this component.") {messageDetailsFlags = Just $ InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral]}
    )

-- | Take a message to send when a user that is not the one that created a
-- component, and then parse out a user id, and then get the interaction
-- requestor's userid, check if they match, and if they don't then send a
-- message. Regardless, parse out the given function. If it _does_ match, run
-- the parsed function.
--
-- Adds eof to the end to ensure all the data is parsed.
onlyAllowRequestor' :: forall f. (PComm f () Interaction MessageDetails) => MessageDetails -> f -> Parser (Interaction -> DatabaseDiscord MessageDetails)
onlyAllowRequestor' msg f = do
  pre <- parseComm prefunc
  f' <- parseComm @f @() @Interaction @MessageDetails f
  parseComm
    ( \i -> do
        isEqual <- pre i
        case isEqual of
          Nothing -> f' i
          Just d -> return d
    )
    <* eof
  where
    prefunc :: UserId -> ParseUserId -> Interaction -> DatabaseDiscord (Maybe MessageDetails)
    prefunc uid (ParseUserId u) i =
      if uid == u
        then return Nothing
        else
          interactionResponseCustomMessage
            i
            msg
            >> return (Just def)
