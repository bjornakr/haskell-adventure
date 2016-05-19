module Conversation where
    import Data.Char (digitToInt)
    import qualified Data.Map as Map
    import System.IO (hFlush, stdout)
    import Entity
    import ConversationTypes
    import CoreTypes
    import Core

    showConversation :: Conversation -> GameState -> String
    showConversation conv@Conversation { cResponseSetId = rsId } GameState { conversationState = ConversationState { stateResponseSets = responseSets }} =
        case findEntityById rsId responseSets of
            Nothing -> error $ "Could not find response set " ++ rsId
            Just rs -> show conv ++ "\n\n" ++ show rs

    getResponses :: Conversation -> ConversationState -> [Response]
    getResponses Conversation { cResponseSetId = rsId } ConversationState { stateResponseSets = responseSets } =
        case findEntityById rsId responseSets of
            Nothing -> error $ "Could not find response set " ++ rsId
            Just ResponseSet { responses = r } -> r
    getResponses ConversationStopper {} _ = []

    respond :: Response -> [Conversation] -> Maybe Conversation
    respond Response { nextConversationId = cId } = findEntityById cId

    num2resp :: Int -> [Response] -> Maybe Response
    num2resp x responses
        | x < 1 = Nothing
        | x > length responses = Nothing
        | otherwise = Just (responses !! (x-1))

    removeResponse :: ResponseId -> ConversationState -> ConversationState
    removeResponse rId state@ConversationState { stateResponseSets = responseSets } = 
        state { stateResponseSets = map (remove rId) responseSets }
        where
            remove :: ResponseId -> ResponseSet -> ResponseSet
            remove rId responseSet@ResponseSet { responses = rs } =
                responseSet { responses = removeEntityById rId rs }

    addResponse :: ResponseSetId -> Response -> ConversationState -> ConversationState
    addResponse rsId response state@ConversationState { stateResponseSets = responseSets } =
        state { stateResponseSets = map (add rsId response) responseSets }
        where
            add :: ResponseSetId -> Response -> ResponseSet -> ResponseSet
            add rsId response responseSet@ResponseSet { responses = resps }
                | rsId == getId responseSet = responseSet { responses = response:resps }
                | otherwise = responseSet

    replaceResponse :: ResponseSetId -> ResponseId -> Response -> ConversationState -> ConversationState
    replaceResponse rsId rId newResponse = addResponse rsId newResponse . removeResponse rId

    insertInConversationStartMap :: String -> String -> ConversationState -> ConversationState
    insertInConversationStartMap key val state@ConversationState { conversationStartMap = sm } = 
        state { conversationStartMap = Map.insert key val sm }

-- TODO: When gamestate is included
    --showResponse :: Response -> ConversationState -> Bool
    --showResponse responseId _ =
    --    case responseId of
    --        _ -> True

    processResponse' :: ResponseSetId -> ResponseId -> ConversationState -> ConversationState
    processResponse' rsId rId =
        case rId of
            "ROGRES1c" -> insertInConversationStartMap "ROGER" "ROGER3"
            "ROGRES3a" -> replaceResponse "ROGRES3" rId rognew1
            "ROGRES3b" -> addResponse "ROGRES3" rognew2 . removeResponse rId            
            _ -> id
        where 
            rognew1 = Response "ROGNEW1" "ROGER5" "Who did you say your contact in Ergo group was?"
            rognew2 = Response "ROGNEW2" "ROGER7" "Are you sure Dorothy didn't mention anything unusual?"

    processResponse :: ResponseSetId -> ResponseId -> GameState -> GameState
    processResponse rsId rId state@GameState { conversationState = conversationState } =
        state { conversationState = processResponse' rsId rId conversationState }

    talkx :: Conversation -> GameState -> IO GameState
    talkx conv@Conversation { cResponseSetId = rsId } gameState@GameState { conversationState = conversationState } = do
        putStrLn $ showConversation conv gameState
        putStr "> "
        hFlush stdout
        choice <- readLn :: IO Int
        let response = num2resp choice (getResponses conv conversationState)
        case response of
            Nothing -> talkx conv gameState
            Just resp@Response { responseId = rId, nextConversationId = ncId } -> do
                let newGameState = processResponse rsId rId gameState
                talk ncId newGameState

    talk :: ConversationId -> GameState -> IO GameState
    talk conversationId state@GameState { conversationState = ConversationState { stateConversations = conversations }} =
        case findEntityById conversationId conversations of
            Nothing -> error $ "Could not find conversation id \"" ++ conversationId ++ "\"."
            Just conv@ConversationStopper {} -> print conv >> return state
            Just conv -> talkx conv state

    initiateConversation :: String -> GameState -> IO GameState
    initiateConversation conversationKey state@GameState { conversationState = ConversationState { conversationStartMap = conversationStartMap }} =
        case Map.lookup conversationKey conversationStartMap of
            Nothing -> error $ "Could not find conversation key \"" ++ conversationKey ++ "\"."
            Just conversationId -> talk conversationId state


