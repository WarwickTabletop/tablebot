-- |
-- Module      : Tablebot.Plugins.Roll.Plugin
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Tablebot.Plugins.Roll.Plugin (rollPlugin) where

import Control.Monad.Writer (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack, replicate, unpack)
import qualified Data.Text as T
import Discord.Interactions
import Discord.Types
  ( ButtonStyle (ButtonStyleSecondary),
    ComponentActionRow (ComponentActionRowButton),
    ComponentButton (ComponentButton),
    Emoji (Emoji),
    GuildMember (memberUser),
    Message (messageAuthor),
    User (userId),
  )
import Tablebot.Internal.Handler.Command (makeBundleReadable)
import Tablebot.Plugins.Roll.Dice
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Utility
import Tablebot.Utility.Discord (interactionResponseComponentsUpdateMessage, interactionResponseCustomMessage, sendCustomMessage, toMention, toMention')
import Tablebot.Utility.Exception (BotException (InteractionException, ParserException), throwBot)
import Tablebot.Utility.Parser (inlineCommandHelper)
import Tablebot.Utility.SmartParser (PComm (parseComm), Quoted (Qu, quote), pars)
import Text.Megaparsec (MonadParsec (try), choice, errorBundlePretty, parse, (<?>))
import Text.RawString.QQ (r)

-- | The basic execution function for rolling dice. Both the expression and message are
-- optional. If the expression is not given, then the default roll is used.
rollDice'' :: Maybe (Either ListValues Expr) -> Maybe (Quoted Text) -> Text -> DatabaseDiscord Text
rollDice'' e' t u = do
  let e = fromMaybe (Right defaultRoll) e'
  (vs, ss) <- case e of
    (Left a) -> liftIO $ first Left <$> evalList a
    (Right b) -> liftIO $ first Right <$> evalInteger b
  let msg = makeMsg vs ss
  if countFormatting msg < 199
    then return msg
    else return (makeMsg (simplify vs) (prettyShow e <> " `[could not display rolls]`"))
  where
    dsc = maybe ": " (\(Qu t') -> " \"" <> t' <> "\": ") t
    baseMsg = u <> " rolled" <> dsc
    makeLine (i, s) = pack (show i) <> Data.Text.replicate (max 0 (6 - length (show i))) " " <> " ⟵ " <> s
    makeMsg (Right v) s = baseMsg <> s <> ".\nOutput: " <> pack (show v)
    makeMsg (Left []) _ = baseMsg <> "No output."
    makeMsg (Left ls) ss
      | all (T.null . snd) ls = baseMsg <> ss <> "\nOutput: {" <> intercalate ", " (pack . show . fst <$> ls) <> "}"
      | otherwise = baseMsg <> ss <> "\n  " <> intercalate "\n  " (makeLine <$> ls)
    simplify (Left ls) = Left $ fmap (\(i, _) -> (i, "...")) ls
    simplify li = li
    countFormatting s = (`div` 4) $ T.foldr (\c cf -> cf + (2 * fromEnum (c == '`')) + fromEnum (c `elem` ['~', '_', '*'])) 0 s

rollDice' :: Maybe (Either ListValues Expr) -> Maybe (Quoted Text) -> Message -> DatabaseDiscord MessageDetails
rollDice' e t m = do
  (msg, buttons) <- getMessagePieces e t (toMention $ messageAuthor m)
  return ((messageJustText msg) {messageDetailsComponents = buttons})

getMessagePieces :: Maybe (Either ListValues Expr) -> Maybe (Quoted Text) -> Text -> DatabaseDiscord (Text, Maybe [ComponentActionRow])
getMessagePieces e t u = do
  msg <- rollDice'' e t u
  return
    ( msg,
      Just
        [ ComponentActionRowButton
            [ ComponentButton ((("roll`" <> u) `appendIf` (prettyShow <$> e)) `appendIf` (quote <$> t)) False ButtonStyleSecondary "Reroll" (Just (Emoji (Just 0) "🎲" Nothing Nothing Nothing (Just False)))
            ]
        ]
    )
  where
    appendIf t' Nothing = t' <> "`"
    appendIf t' (Just e') = t' <> "`" <> e'

rerollInteraction :: Interaction -> DatabaseDiscord ()
rerollInteraction i@InteractionComponent {interactionDataComponent = Just (InteractionDataComponentButton cid)}
  | length opts /= 4 = throwBot $ InteractionException "could not process button click"
  | maybe True (\u -> toMention u /= opts !! 1) getUser = interactionResponseCustomMessage i ((messageJustText "Hey, that isn't your button to press!") {messageDetailsFlags = Just $ InteractionCallbackDataFlags [InteractionCallbackDataFlagEphermeral]})
  | otherwise = case opts of
    [_, uid, "", ""] -> do
      (msg, button) <- getMessagePieces Nothing Nothing uid
      interactionResponseComponentsUpdateMessage i ((messageJustText msg) {messageDetailsComponents = button})
    [_, uid, "", qt] -> do
      (msg, button) <- getMessagePieces Nothing (Just (Qu qt)) uid
      interactionResponseComponentsUpdateMessage i ((messageJustText msg) {messageDetailsComponents = button})
    [_, uid, e, ""] -> do
      let e' = parse pars "" e
      case e' of
        Left _ -> throwBot $ InteractionException "could not process button click"
        Right e'' -> do
          (msg, button) <- getMessagePieces (Just e'') Nothing uid
          interactionResponseComponentsUpdateMessage i ((messageJustText msg) {messageDetailsComponents = button})
    [_, uid, e, qt] -> do
      let e' = parse pars "" e
      case e' of
        Left _ -> throwBot $ InteractionException "could not process button click"
        Right e'' -> do
          (msg, button) <- getMessagePieces (Just e'') (Just (Qu qt)) uid
          interactionResponseComponentsUpdateMessage i ((messageJustText msg) {messageDetailsComponents = button})
    _ -> throwBot $ InteractionException "could not process button click"
  where
    opts = T.split (== '`') cid
    getUser = maybe (interactionUser i) memberUser (interactionMember i)
rerollInteraction _ = return ()

rollSlashCommandInteraction :: Interaction -> DatabaseDiscord ()
rollSlashCommandInteraction i@InteractionApplicationCommand {interactionDataApplicationCommand = Just InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "roll", interactionDataApplicationCommandOptions = opts}} = do
  e <- mapM parseExpr expr
  (msg, buttons) <- getMessagePieces e (Qu <$> qt) (toMention' getUser)
  interactionResponseCustomMessage i ((messageJustText msg) {messageDetailsComponents = buttons})
  where
    findWhere s = opts >>= \(InteractionDataApplicationCommandOptionsValues values) -> lookup s $ (\v -> (interactionDataApplicationCommandOptionValueName v, interactionDataApplicationCommandOptionValueValue v)) <$> values
    expr = findWhere "expression" >>= getText
    qt = findWhere "quote" >>= getText
    getText (ApplicationCommandInteractionDataValueString t) = Just t
    getText _ = Nothing
    -- TODO: work out why this exception handling isn't working
    parseExpr expr' = case parse pars "" expr' of
      Right p -> return p
      Left e ->
        let (errs, title) = makeBundleReadable e
         in throwBot $ ParserException title $ "```\n" ++ errorBundlePretty errs ++ "```"
    getUser = maybe 0 userId $ maybe (interactionUser i) memberUser (interactionMember i)
rollSlashCommandInteraction _ = return ()

-- TODO: tie together creating the application command and the handler for it so that they cannot be separated
-- TODO: comment
rollSlashCommand :: Maybe CreateApplicationCommand
rollSlashCommand =
  createApplicationCommandChatInput "roll" "roll some dice with a description" >>= \cac ->
    return $
      cac
        { createApplicationCommandOptions =
            Just $
              ApplicationCommandOptionsValues
                [ ApplicationCommandOptionValueString "expression" "What expression is being evaluated (list or integer)" Nothing Nothing Nothing,
                  ApplicationCommandOptionValueString "quote" "What message is associated with this roll" Nothing Nothing Nothing
                ]
        }

-- | Manually creating parser for this command, since SmartCommand doesn't work fully for
-- multiple Maybe values
rollDiceParser :: Parser (Message -> DatabaseDiscord ())
rollDiceParser = choice (try <$> options)
  where
    options =
      [ parseComm (\lv -> rollDice' (Just lv) Nothing),
        parseComm (rollDice' Nothing Nothing),
        try (parseComm (\lv qt -> rollDice' (Just lv) (Just qt))) <?> "",
        try (parseComm (rollDice' Nothing . Just)) <?> ""
      ]

-- | Basic command for rolling dice.
rollDice :: Command
rollDice = Command "roll" rollDiceParser []

-- | Rolling dice inline.
rollDiceInline :: InlineCommand
rollDiceInline = inlineCommandHelper "[|" "|]" pars (\e m -> rollDice' (Just e) Nothing m >>= sendCustomMessage m)

-- | Help page for rolling dice, with a link to the help page.
rollHelp :: HelpPage
rollHelp =
  HelpPage
    "roll"
    ["r"]
    "roll dice and do maths"
    rollHelpText
    []
    None

-- | A large chunk of help text for the roll command.
rollHelpText :: Text
rollHelpText =
  pack $
    [r|**Roll**
Given an expression, evaluate the expression. Can roll inline using |]
      ++ "`[|to roll|]`."
      ++ [r| Can use `r` instead of `roll`.

This supports addition, subtraction, multiplication, integer division, exponentiation, parentheses, dice of arbitrary size, dice with custom sides, rerolling dice once on a condition, rerolling dice indefinitely on a condition, keeping or dropping the highest or lowest dice, keeping or dropping dice based on a condition, operating on lists, and using functions like |]
      ++ unpack (intercalate ", " integerFunctionsList)
      ++ [r| (which return integers), or functions like |]
      ++ unpack (intercalate ", " listFunctionsList)
      ++ [r| (which return lists).

To see a full list of uses and options, please go to <https://github.com/WarwickTabletop/tablebot/blob/main/docs/Roll.md>.

*Usage:*
  - `roll 1d20` -> rolls a twenty sided die and returns the outcome
  - `roll 3d6 + 5d4` -> sums the result of rolling three d6s and five d4s
  - `roll 2d20kh1` -> keeps the highest value out of rolling two d20s
  - `roll 5d10dl4` -> roll five d10s and drop the lowest four
|]

-- | Command for generating characters.
genchar :: Command
genchar = Command "genchar" (snd $ head rpgSystems') (toCommand <$> rpgSystems')
  where
    doDiceRoll (nm, lv) = (nm, parseComm $ rollDice' (Just (Left lv)) (Just (Qu ("genchar for " <> nm))))
    rpgSystems' = doDiceRoll <$> rpgSystems
    toCommand (nm, ps) = Command nm ps []

-- | List of supported genchar systems and the dice used to roll for them
rpgSystems :: [(Text, ListValues)]
rpgSystems =
  [ ("dnd", MultipleValues (Value 6) (DiceBase (Dice (NBase (Value 4)) (Die (Value 6)) (Just (DieOpRecur (DieOpOptionKD Drop (Low (Value 1))) Nothing))))),
    ("wfrp", MultipleValues (Value 8) (NBase (NBParen (Paren (Add (promote (Value 20)) (promote (Die (Value 10))))))))
  ]

-- | Small help page for gen char.
gencharHelp :: HelpPage
gencharHelp =
  HelpPage
    "genchar"
    []
    "generate stat arrays for some systems"
    ("**Genchar**\nCan be used to generate stat arrays for certain systems.\n\nCurrently supported systems: " <> intercalate ", " (fst <$> rpgSystems) <> ".\n\n*Usage:* `genchar`, `genchar dnd`")
    []
    None

-- | @rollPlugin@ assembles the command into a plugin.
rollPlugin :: Plugin
rollPlugin =
  (plug "roll")
    { commands = [rollDice, commandAlias "r" rollDice, genchar],
      helpPages = [rollHelp, gencharHelp],
      inlineCommands = [rollDiceInline],
      onComponentInteractionRecvs =
        [ InteractionRecv rerollInteraction
        ],
      applicationCommands = [(rollSlashCommand, InteractionRecv rollSlashCommandInteraction)]
    }
