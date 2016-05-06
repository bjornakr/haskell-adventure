module Action.TalkTo where
    import Entity
    import Core
    
    talkTo :: Actor -> GameState -> ActionResult
    talkTo actor = ConversationTrigger "ROGER1"
