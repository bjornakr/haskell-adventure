import Data.Char (digitToInt)
import System.IO (hFlush, stdout)

type ConversationId = String
data Response = Response String ConversationId
data Conversation = Conversation ConversationId String [Response] | ConversationEnd ConversationId String

instance Show Conversation where
    show (Conversation _ text responses) =
        text ++ "\n\n" ++ (concat (map showResponse (zip [1..] responses)))
    show (ConversationEnd _ text) = text

instance Show Response where
    show (Response _ text) = text

getId :: Conversation -> ConversationId
getId (Conversation conversationId _ _) = conversationId
getId (ConversationEnd conversationId _) = conversationId

getResponses :: Conversation -> [Response]
getResponses (Conversation _ _ responses) = responses
getResponses (ConversationEnd _ _) = []

showResponse :: (Int, Response) -> String
showResponse (no, response) = (show no) ++ ") " ++ show response ++ "\n"


respond :: Response -> [Conversation] -> Maybe Conversation
respond (Response conversationId _) conversations = findConversation conversationId conversations

num2resp :: Int -> [Response] -> Maybe Response
num2resp x responses
    | x < 1 = Nothing
    | x > (length responses) = Nothing
    | otherwise = Just (responses !! (x-1))

findConversation :: ConversationId -> [Conversation] -> Maybe Conversation
findConversation conversationId [] = Nothing
findConversation conversationId (conversation:conversations)
    | conversationId == getId conversation = Just conversation
    | otherwise = findConversation conversationId conversations

h1 = Conversation "H1" "Hello. How are you doing?" [
    Response "H11" "Great",
    Response "H11" "Not too bad",
    Response "H12" "Feeling down"
    ]

h2 = Conversation "H11" "That's good to hear.\n\n* He gives you an apple." [
    Response "HEND" "Good bye, I gotta run!",
    Response "HEND2" "*Run away*"
    ]

hend = ConversationEnd "HEND" "Ok, see you around!"

hend2 = ConversationEnd "HEND2" "* You run away before he can respond."



talk :: Maybe Conversation -> [Conversation] -> IO ()
talk Nothing _ = error "Missing conversation"
talk (Just conv@(ConversationEnd _ _)) _ = putStrLn $ show conv
talk (Just conversation) conversations = do
    putStrLn $ show conversation
    putStr "> "
    hFlush stdout
    respNum <- readLn :: IO Int
    let choice = num2resp respNum (getResponses conversation)    
    case choice of
        Just (Response conversationId _) -> talk (findConversation conversationId conversations) conversations
        Nothing -> do
            putStrLn "Eh, what was that?"
            talk (Just conversation) conversations


main = do
    talk (Just h1) [h1, h2, hend, hend2]
    return ()