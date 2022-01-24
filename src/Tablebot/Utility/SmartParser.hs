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
import Data.Proxy (Proxy (..))
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Discord.Interactions
import Discord.Types
import GHC.OldList (find)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Tablebot.Internal.Handler.Command (parseValue)
import Tablebot.Utility.Discord (interactionResponseComponentsUpdateMessage, interactionResponseCustomMessage, sendCustomMessage)
import Tablebot.Utility.Exception (BotException (InteractionException, ParserException), catchBot, embedError, throwBot)
import Tablebot.Utility.Parser
import Tablebot.Utility.Types
import Text.Megaparsec (MonadParsec (eof, try), chunk, many, optional, (<?>), (<|>))

class Context a where
  contextUserId :: a -> ParseUserId

instance Context Message where
  contextUserId = ParseUserId . userId . messageAuthor

instance Context Interaction where
  contextUserId i = ParseUserId $ maybe 0 userId (maybe (interactionUser i) memberUser (interactionMember i))

-- | @PComm@ defines function types that we can automatically turn into parsers
-- by composing a parser per input of the function provided.
-- For example, @Int -> Maybe Text -> Message -> DatabaseDiscord s ()@ builds a
-- parser that reads in an @Int@, then some optional @Text@, and then uses
-- those to run the provided function with the arguments parsed and the message
-- itself.
class PComm commandty s context returns where
  parseComm :: (Context context) => commandty -> Parser (context -> EnvDatabaseDiscord s returns)

-- TODO: verify that all the parsers for PComm actually work
-- As a base case, remove the spacing and check for eof.
instance {-# OVERLAPPING #-} PComm (t -> EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> eof >> return comm

instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm >>= sendCustomMessage m)

instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> eof >> return (const comm)

instance {-# OVERLAPPING #-} PComm (Message -> EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm m >>= sendCustomMessage m)

-- TODO: verify that this second base case is no longer needed
-- -- Second base case is the single argument - no trailing space is wanted so we
-- -- have to specify this case.
-- instance {-# OVERLAPPING #-} CanParse a => PComm (a -> Message -> EnvDatabaseDiscord s ()) s where
--   parseComm comm = do
--     this <- pars @a
--     parseComm (comm this)

-- Recursive case is to parse the domain of the function type, then the rest.
instance {-# OVERLAPPABLE #-} (CanParse a, PComm as s t r) => PComm (a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (comm this)

instance {-# OVERLAPPABLE #-} (PComm (t -> as) s t r) => PComm (t -> t -> as) s t r where
  parseComm comm = parseComm (\m -> comm m m)

instance {-# OVERLAPPABLE #-} (Context t, CanParse a, PComm (t -> as) s t r) => PComm (t -> a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (`comm` this)

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
  pars = (Left <$> pars @a) <|> (Right <$> pars @b)

-- TODO: automate creation of tuple instances using TemplateHaskell
instance (CanParse a, CanParse b) => CanParse (a, b) where
  pars = do
    x <- pars @a
    skipSpace1
    y <- pars @b
    return (x, y)

instance (CanParse a, CanParse b, CanParse c) => CanParse (a, b, c) where
  pars = do
    x <- pars @a
    skipSpace1
    y <- pars @b
    skipSpace1
    z <- pars @c
    return (x, y, z)

instance (CanParse a, CanParse b, CanParse c, CanParse d) => CanParse (a, b, c, d) where
  pars = do
    x <- pars @a
    skipSpace1
    y <- pars @b
    skipSpace1
    z <- pars @c
    skipSpace1
    w <- pars @d
    return (x, y, z, w)

instance (CanParse a, CanParse b, CanParse c, CanParse d, CanParse e) => CanParse (a, b, c, d, e) where
  pars = do
    x <- pars @a
    skipSpace1
    y <- pars @b
    skipSpace1
    z <- pars @c
    skipSpace1
    w <- pars @d
    skipSpace1
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
  pars = (WErr <$> pars @x) <?> symbolVal (Proxy :: Proxy err)

-- | Parsing implementation for all integral types
-- Overlappable due to the really flexible head state
instance {-# OVERLAPPABLE #-} (Integral a, Read a) => CanParse a where
  pars = integer

instance CanParse Double where
  pars = double

instance CanParse () where
  pars = eof

instance CanParse Snowflake where
  pars = Snowflake . fromInteger <$> pars

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput a = ROI a

instance IsString a => CanParse (RestOfInput a) where
  pars = ROI . fromString <$> untilEnd

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput1 a = ROI1 a

instance IsString a => CanParse (RestOfInput1 a) where
  pars = ROI1 . fromString <$> untilEnd1

newtype ParseUserId = ParseUserId UserId

-- | Labelled value for use with smart commands.
newtype Labelled (name :: Symbol) (desc :: Symbol) a = Labelled a

-- | Easily make a labelled value.
labelValue :: forall n d a. a -> Labelled n d a
labelValue = Labelled @n @d

getLabelValues :: forall n d a. (KnownSymbol n, KnownSymbol d) => Proxy (Labelled n d a) -> (Text, Text)
getLabelValues _ = (pack (symbolVal (Proxy :: Proxy n)), pack (symbolVal (Proxy :: Proxy d)))

instance (CanParse a) => CanParse (Labelled n d a) where
  pars = labelValue <$> pars

-- | @noArguments@ is a type-specific alias for @parseComm@ for commands that
-- have no arguments (thus making it extremely clear).
noArguments :: (Message -> EnvDatabaseDiscord d ()) -> Parser (Message -> EnvDatabaseDiscord d ())
noArguments = parseComm

--------------------------------------------------------------------------------
-- Interactions stuff
----

makeApplicationCommandPair :: forall t s. (MakeAppComm t, ProcessAppComm t s) => Text -> Text -> t -> Maybe (EnvApplicationCommandRecv s)
makeApplicationCommandPair name desc f = do
  cac <- makeSlashCommand name desc (Proxy :: Proxy t)
  return $ ApplicationCommandRecv cac (processAppComm f)

makeSlashCommand :: (MakeAppComm t) => Text -> Text -> Proxy t -> Maybe CreateApplicationCommand
makeSlashCommand name desc p =
  createApplicationCommandChatInput name desc >>= \cac ->
    return $
      cac
        { createApplicationCommandOptions = Just $ ApplicationCommandOptionsValues $ makeAppComm p
        }

class MakeAppComm commandty where
  makeAppComm :: Proxy commandty -> [ApplicationCommandOptionValue]

-- As a base case, no more arguments
instance {-# OVERLAPPING #-} MakeAppComm (EnvDatabaseDiscord s MessageDetails) where
  makeAppComm _ = []

instance {-# OVERLAPPABLE #-} (MakeAppComm mac, MakeAppCommArg ty) => MakeAppComm (ty -> mac) where
  makeAppComm _ = makeAppCommArg (Proxy :: Proxy ty) : makeAppComm (Proxy :: Proxy mac)

instance {-# OVERLAPPABLE #-} (MakeAppComm mac) => MakeAppComm (ParseUserId -> mac) where
  makeAppComm _ = makeAppComm (Proxy :: Proxy mac)

class MakeAppCommArg commandty where
  makeAppCommArg :: Proxy commandty -> ApplicationCommandOptionValue

instance (KnownSymbol name, KnownSymbol desc) => MakeAppCommArg (Labelled name desc Text) where
  makeAppCommArg l = ApplicationCommandOptionValueString n d (Just True) Nothing Nothing
    where
      (n, d) = getLabelValues l

instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Maybe t)) where
  makeAppCommArg _ =
    (makeAppCommArg (Proxy :: Proxy (Labelled name desc t)))
      { applicationCommandOptionValueRequired = Just False
      }

instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Quoted t)) where
  makeAppCommArg _ = makeAppCommArg (Proxy :: Proxy (Labelled name desc t))

-- As a base case, send the message produced

class ProcessAppComm commandty s where
  processAppComm :: commandty -> Interaction -> EnvDatabaseDiscord s ()
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

instance {-# OVERLAPPING #-} ProcessAppComm (EnvDatabaseDiscord s MessageDetails) s where
  processAppComm comm i = comm >>= interactionResponseCustomMessage i

instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (Interaction -> pac) s where
  processAppComm comm i = processAppComm (comm i) i

instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s, ProcessAppCommArg ty s) => ProcessAppComm (ty -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {interactionDataApplicationCommand = Just InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandOptions = (Just (InteractionDataApplicationCommandOptionsValues values))}} = do
    t <- processAppCommArg values
    processAppComm (comm t) i
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (ParseUserId -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {} =
    case getUser of
      Nothing -> throwBot $ InteractionException "could not process args to application command"
      Just uid -> processAppComm (comm (ParseUserId uid)) i
    where
      getUser = userId <$> maybe (interactionUser i) memberUser (interactionMember i)
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

class ProcessAppCommArg t s where
  processAppCommArg :: [InteractionDataApplicationCommandOptionValue] -> EnvDatabaseDiscord s t

getValue :: String -> [InteractionDataApplicationCommandOptionValue] -> Maybe ApplicationCommandInteractionDataValue
getValue t is = interactionDataApplicationCommandOptionValueValue <$> find ((== pack t) . interactionDataApplicationCommandOptionValueName) is

instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Text) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (ApplicationCommandInteractionDataValueString t) -> return $ labelValue t
    _ -> throwBot $ InteractionException "could not find required parameter"

instance (KnownSymbol name, KnownSymbol desc, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Quoted t)) s where
  processAppCommArg is = processAppCommArg @(Labelled name desc t) is >>= \(Labelled a) -> return (labelValue (Qu a))

instance (KnownSymbol name, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Maybe t)) s where
  processAppCommArg is = do
    let result = processAppCommArg is :: EnvDatabaseDiscord s (Labelled name desc t)
    ( do
        (Labelled l) <- result
        return (labelValue (Just l))
      )
      `catchBot` const (return $ labelValue Nothing)

-- | Given a function that can be processed to create a parser, create an action
-- for it using the helper.
--
-- If the boolean is true, the message the component is from is updated. Else,
-- a message is sent as the interaction response.
processComponentInteraction :: (PComm f s Interaction MessageDetails) => f -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction f = processComponentInteraction' (parseComm f)

-- TODO: comment what is given to the parser

-- | Given a parser that, when run, returns a function taking an interaction
-- and returns a database action on some MessageDetails, run the action.
--
-- If the boolean is true, the message the component is from is updated. Else,
-- a message is sent as the interaction response.
processComponentInteraction' :: Parser (Interaction -> EnvDatabaseDiscord s MessageDetails) -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction' compParser updateOriginal i@InteractionComponent {interactionDataComponent = Just idc} = errorCatch $ do
  let componentSend
        | updateOriginal = interactionResponseComponentsUpdateMessage i
        | otherwise = interactionResponseCustomMessage i
  action <- parseValue (skipSpace *> compParser) (interactionDataComponentCustomId idc) >>= ($ i)
  componentSend action
  where
    catchParserException e@(ParserException _ _) = interactionResponseCustomMessage i $ (messageDetailsBasic "something (likely) went wrong when processing a component interaction") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    catchParserException e = interactionResponseCustomMessage i $ (messageDetailsBasic "") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    errorCatch = (`catch` catchParserException)
processComponentInteraction' _ _ _ = throwBot $ InteractionException "could not process component interaction"
