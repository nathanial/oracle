/-
  Oracle/Reactive/Conversation.lean

  ConversationManager for multi-turn conversation management with reactive state.
  Provides observable conversation history and streaming responses.
-/
import Reactive.Host.Spider.Core
import Oracle.Reactive.Types
import Oracle.Reactive.Client

namespace Oracle.Reactive

open Reactive.Host
open Oracle

/-- State of the conversation manager -/
inductive ConversationState where
  | idle
  | sending
  | streaming
  | error : OracleError → ConversationState
  deriving Repr

namespace ConversationState

instance : Inhabited ConversationState where
  default := .idle

instance : BEq ConversationState where
  beq a b := match a, b with
    | .idle, .idle => true
    | .sending, .sending => true
    | .streaming, .streaming => true
    | .error e1, .error e2 => decide (toString e1 = toString e2)
    | _, _ => false

def isIdle : ConversationState → Bool
  | .idle => true
  | _ => false

def isBusy : ConversationState → Bool
  | .sending => true
  | .streaming => true
  | _ => false

def isError : ConversationState → Bool
  | .error _ => true
  | _ => false

end ConversationState

/-- Manages a multi-turn conversation with observable state.

    Provides:
    - Observable conversation history
    - Observable streaming state
    - Event when assistant responds
    - Methods to send messages and control the conversation -/
structure ConversationManager where
  /-- Observable conversation history -/
  conversation : Dyn Conversation
  /-- Observable current state -/
  state : Dyn ConversationState
  /-- Fires with assistant response content when a response completes -/
  assistantResponded : Evt String
  /-- Current streaming output (if any) -/
  currentStream : Dyn (Option StreamingRequestOutput)
  /-- Send a user message and get a streaming response -/
  sendMessage : String → IO Unit
  /-- Cancel the current streaming response (if any) -/
  cancelCurrent : IO Unit
  /-- Clear conversation history (keeps system prompt) -/
  clear : IO Unit
  /-- Add a message to history without sending to API -/
  addMessage : Message → IO Unit

namespace ConversationManager

/-- Create a new ConversationManager.

    @param client The ReactiveClient to use for API requests
    @param systemPrompt Optional system prompt to include at the start of conversations -/
def new (client : ReactiveClient) (systemPrompt : Option String := none) : SpiderM ConversationManager := ⟨fun env => do
  -- Create conversation state
  let initialConv := match systemPrompt with
    | some prompt => Conversation.withSystemPrompt prompt
    | none => Conversation.empty

  let (convDyn, updateConv) ← createDynamic env.timelineCtx initialConv
  let (stateDyn, updateState) ← createDynamic env.timelineCtx ConversationState.idle
  let (respondedEvt, fireResponded) ← Reactive.Event.newTrigger env.timelineCtx
  let (streamDyn, updateStream) ← createDynamic env.timelineCtx (none : Option StreamingRequestOutput)

  let framedUpdateConv := fun v => env.withFrame (updateConv v)
  let framedUpdateState := fun v => env.withFrame (updateState v)
  let framedFireResponded := fun v => env.withFrame (fireResponded v)
  let framedUpdateStream := fun v => env.withFrame (updateStream v)

  -- Current request cancellation
  let currentCancelRef ← IO.mkRef (pure () : IO Unit)
  let conversationRef ← IO.mkRef initialConv

  -- Send message implementation
  let sendMessageImpl := fun (userMessage : String) => do
    -- Cancel any current request
    let currentCancel ← currentCancelRef.get
    currentCancel

    -- Add user message to conversation
    let conv ← conversationRef.get
    let conv' := conv.addUser userMessage
    conversationRef.set conv'
    framedUpdateConv conv'
    framedUpdateState .sending

    -- Create the chat request
    let req : ChatRequest := {
      model := client.client.config.model
      messages := conv'.allMessages
    }

    -- Start streaming request
    let output ← (client.chatStream req).run env
    currentCancelRef.set output.cancel
    framedUpdateStream (some output)
    framedUpdateState .streaming

    -- Subscribe to completion to update conversation
    let unsub ← Reactive.Event.subscribe output.completed fun streamState => do
      let currentConv ← conversationRef.get
      let assistantMsg := Message.assistant streamState.content
      let newConv := currentConv.addMessage assistantMsg
      conversationRef.set newConv
      framedUpdateConv newConv
      framedUpdateState .idle
      framedUpdateStream none
      framedFireResponded streamState.content

    env.currentScope.register unsub

    -- Subscribe to errors
    let unsubErr ← Reactive.Event.subscribe output.errored fun err => do
      framedUpdateState (.error err)
      framedUpdateStream none

    env.currentScope.register unsubErr

  -- Cancel current implementation
  let cancelCurrentImpl := do
    let cancel ← currentCancelRef.get
    cancel
    currentCancelRef.set (pure ())
    framedUpdateStream none
    let state ← stateDyn.sample
    if state.isBusy then
      framedUpdateState .idle

  -- Clear implementation
  let clearImpl := do
    cancelCurrentImpl
    let conv ← conversationRef.get
    let cleared := conv.clear
    conversationRef.set cleared
    framedUpdateConv cleared

  -- Add message implementation
  let addMessageImpl := fun (msg : Message) => do
    let conv ← conversationRef.get
    let conv' := conv.addMessage msg
    conversationRef.set conv'
    framedUpdateConv conv'

  pure {
    conversation := convDyn
    state := stateDyn
    assistantResponded := respondedEvt
    currentStream := streamDyn
    sendMessage := sendMessageImpl
    cancelCurrent := cancelCurrentImpl
    clear := clearImpl
    addMessage := addMessageImpl
  }
⟩

/-- Create a ConversationManager with a system prompt -/
def withSystemPrompt (client : ReactiveClient) (systemPrompt : String)
    : SpiderM ConversationManager :=
  new client (some systemPrompt)

end ConversationManager

end Oracle.Reactive
