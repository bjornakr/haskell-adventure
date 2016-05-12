module Action.TalkTo where
    import Entity
    import CoreTypes
    import Core
    
    talkTo :: Actor -> GameState -> ActionResult
    talkTo actor = ConversationTrigger "ROGER"
