import Data.Char (digitToInt)
import System.IO (hFlush, stdout)
import Entity

type ConversationId = String
type ResponseId = String

data Response = Response {
    responseId :: String,
    nextConversationId :: String,
    responseText :: String
} deriving (Eq)

data ResponseSet = ResponseSet {
    responseSetId :: String,
    responses :: [Response]
} deriving (Eq)

data Conversation = 
    Conversation {
        conversationId :: String,
        conversationText :: String,
        cResponseSetId :: String
    } 
    | ConversationStopper {
        conversationStopperId :: String,
        conversationStopperText :: String
    } deriving (Eq)


data ConversationState = ConversationState [Conversation] [ResponseSet]

instance Show Response where
    show r = responseText r

instance Show ResponseSet where
    show ResponseSet { responses = rs } = (concat (map showResponse (zip [1..] rs)))
        where showResponse (no, response) = (show no) ++ ") " ++ show response ++ "\n"

instance Show Conversation where
    show (Conversation { conversationText = text }) = text
    show (ConversationStopper { conversationStopperText = text }) = text

instance Entity Response where
    getId (Response { responseId = i }) = i
    getDescription response = show response

instance Entity ResponseSet where
    getId (ResponseSet { responseSetId = i }) = i
    getDescription responseSet = getId responseSet

instance Entity Conversation where
    getId (Conversation { conversationId = i }) = i
    getId (ConversationStopper { conversationStopperId = i }) = i
    getDescription conversation = show conversation

showConversation :: ConversationState -> Conversation -> String
showConversation (ConversationState _ conversationSets) conv@(Conversation { cResponseSetId = rsId }) =
    let responseSet = findEntityById rsId conversationSets in
        case responseSet of
            Nothing -> error $ "Could not find response set " ++ rsId
            (Just rs) -> (show conv) ++ "\n\n" ++ (show rs)

getResponses :: ConversationState -> Conversation -> [Response]
getResponses (ConversationState _ conversationSets) (Conversation { cResponseSetId = rsId }) =
    let responseSet = findEntityById rsId conversationSets in
        case responseSet of
            Nothing -> error $ "Could not find response set " ++ rsId
            (Just (ResponseSet { responses = r })) -> r
getResponses _ (ConversationStopper {}) = []

respond :: Response -> [Conversation] -> Maybe Conversation
respond (Response { nextConversationId = cId }) conversations = findEntityById cId conversations

num2resp :: Int -> [Response] -> Maybe Response
num2resp x responses
    | x < 1 = Nothing
    | x > (length responses) = Nothing
    | otherwise = Just (responses !! (x-1))

--h1 = Conversation { 
--        conversationId = "H1",
--        conversationText = "Hello. How are you doing?",
--        cResponseSetId = "HR1"
--    }

--hr1 = ResponseSet {
--        responseSetId = "HR1",
--        responses = [
--            Response "H11" "Great",
--            Response "H11" "Not too bad",
--            Response "H12" "Feeling down"
--        ]
--    }

--h2 = Conversation {
--        conversationId = "H11",
--        conversationText = "That's good to hear.\n\n* He gives you an apple.",
--        cResponseSetId = "HR2"
--    }

--hr2 = ResponseSet {
--    responseSetId = "HR2",
--    responses = [
--        Response "HEND" "Good bye, I gotta run!",
--        Response "HEND2" "*Run away*"
--    ]
--}


--hend = ConversationStopper "HEND" "Ok, see you around!"

--hend2 = ConversationStopper "HEND2" "* You run away before he can respond."

a1 = Conversation { 
        conversationId = "A1",
        conversationText = "What can I do for you?",
        cResponseSetId = "AR1"
    }


ar1 = ResponseSet {
        responseSetId = "AR1",
        responses = [
            Response "AR11" "tell-me-about-ruru" "Tell me about Ruru.",
            Response "AR12" "A2" "Would you like an apple?",
            Response "AR13" "AEND" "Goodbye."
        ]
    }

a2 = Conversation {
    conversationId  = "A2",
    conversationText = "Thanks! *Munch munch*",
    cResponseSetId = "AR1"
}

a3 = Conversation {
    conversationId = "tell-me-about-ruru",
    conversationText = "He's a great guy!",
    cResponseSetId = "AR1"
}
 


--removeResponse'' :: ResponseId -> [Response] -> [Response]
--removeResponse'' rId [] = []
--removeResponse'' rIdToRemove (Response { responseId = rId })

removeResponse'' :: ResponseId -> ResponseSet -> ResponseSet
removeResponse'' rId responseSet@(ResponseSet { responses = rs }) =
    responseSet { responses = (removeEntityById rId rs) }
--removeResponse'' rIdToRemove (Response { responseId = rId })


removeResponse' :: ResponseId -> [ResponseSet] -> [ResponseSet]
removeResponse' rId [] = []
removeResponse' rId (rs:rss) = (removeResponse'' rId rs):(removeResponse' rId rss)

removeResponse :: ConversationState -> ResponseId -> ConversationState
removeResponse (ConversationState a responseSets) rId = ConversationState a (removeResponse' rId responseSets)
--removeResponse (ConversationState _ responseSets) rId =



--say :: Response -> [Conversation] -> (Maybe Conversation, [Conversation])
--say (Response { responseId = "AR11" } ) =


--say r c = sayX r c

--sayX (Response { followingConversationId = i }) convs =
--    ((findEntityById i convs), convs)

processResponse :: ConversationState -> ResponseId -> ConversationState
--processResponse state rId@"AR11" = addResponse state rId "Can you tell me about Ruru again?"
processResponse state rId@"AR12" = removeResponse state rId
processResponse state _ = state


talk :: ConversationState -> ConversationId -> IO ()
--talk Nothing _ = error "Missing conversation"
talk state@(ConversationState conversations _) conversationId =
    let conversation = findEntityById conversationId conversations in
        case conversation of
            Nothing -> error $ "Could not find conversation id \"" ++ conversationId ++ "\"."
            Just conv@(ConversationStopper {}) -> putStrLn $ show conv
            Just c -> do
                putStrLn $ showConversation state c
                putStr "> "
                hFlush stdout
                choice <- readLn :: IO Int
                let response = num2resp choice (getResponses state c)    
                case response of
                    Nothing -> do
                        talk state conversationId
                    Just resp@(Response { responseId = rId, nextConversationId = ncId }) -> do
                        let newState = processResponse state rId
                        talk newState ncId


main = do
    talk (ConversationState [a1, a2, a3] [ar1]) "A1"
    return ()
