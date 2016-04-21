import Data.Char (digitToInt)
import System.IO (hFlush, stdout)
import Entity

type ConversationId = String
type ResponseSetId = String

data Response = Response {
    followingConversationId :: ConversationId,
    responseText :: String
} deriving (Eq)

data ResponseSet = ResponseSet {
    responseSetId :: ResponseSetId,
    responses :: [Response]
} deriving (Eq)

data Conversation = 
    Conversation {
        conversationId :: ConversationId,
        conversationText :: String,
        responseSet :: ResponseSet
    } 
    | ConversationStopper {
        conversationStopperId :: ConversationId,
        conversationStopperText :: String
    } deriving (Eq)


instance Show Response where
    show r = responseText r

instance Show ResponseSet where
    show ResponseSet { responses = rs } = (concat (map showResponse (zip [1..] rs)))
        where showResponse (no, response) = (show no) ++ ") " ++ show response ++ "\n"

instance Show Conversation where
    show (Conversation { conversationText = text, responseSet = rs }) = 
        text ++ "\n\n" ++ (show rs)
    show (ConversationStopper { conversationStopperText = text }) = text

instance Entity Conversation where
    getId (Conversation { conversationId = i }) = i
    getId (ConversationStopper { conversationStopperId = i }) = i
    getDescription conversation = show conversation

getResponses :: Conversation -> [Response]
getResponses (Conversation { responseSet = ResponseSet { responses = r } }) = r
getResponses (ConversationStopper {}) = []

respond :: Response -> [Conversation] -> Maybe Conversation
respond (Response conversationId _) conversations = findEntityById conversationId conversations

num2resp :: Int -> [Response] -> Maybe Response
num2resp x responses
    | x < 1 = Nothing
    | x > (length responses) = Nothing
    | otherwise = Just (responses !! (x-1))

h1 = Conversation { 
        conversationId = "H1",
        conversationText = "Hello. How are you doing?",
        responseSet = ResponseSet {
            responseSetId = "HR1",
            responses = [
                Response "H11" "Great",
                Response "H11" "Not too bad",
                Response "H12" "Feeling down"
            ]
        }
    }

h2 = Conversation {
        conversationId = "H11",
        conversationText = "That's good to hear.\n\n* He gives you an apple.",
        responseSet = ResponseSet {
            responseSetId = "HR2",
            responses = [
                Response "HEND" "Good bye, I gotta run!",
                Response "HEND2" "*Run away*"
            ]
        }
    }

hend = ConversationStopper "HEND" "Ok, see you around!"

hend2 = ConversationStopper "HEND2" "* You run away before he can respond."

talk :: Maybe Conversation -> [Conversation] -> IO ()
talk Nothing _ = error "Missing conversation"
talk (Just conv@(ConversationStopper _ _)) _ = putStrLn $ show conv
talk (Just conversation) conversations = do
    putStrLn $ show conversation
    putStr "> "
    hFlush stdout
    respNum <- readLn :: IO Int
    let choice = num2resp respNum (getResponses conversation)    
    case choice of
        Just (Response conversationId _) -> talk (findEntityById conversationId conversations) conversations
        Nothing -> do
            putStrLn "Eh, what was that?"
            talk (Just conversation) conversations


main = do
    talk (Just h1) [h1, h2, hend, hend2]
    return ()
