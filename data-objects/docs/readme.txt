1. Get signal state.
    client send request to state server.
    state server replies to client with states available.

    client sends GetStateRequest => server replies to the client with GetStateResponse

2. Push signal state.
    client send request to state server, with state values to be updated to the target signals.
    state server sets the target signals with new values specified.
    the signals updated create an update event and push to the outbound event listeners.
    state server replies to the client with results for the update.
    event consumers consumes the event, and replies to the event source with results for the event processing.   

    client sends PushStateRequest => server creates StateEvent(s)
                                     server pushes StateEvent(s) to listeners => the listeners consumes the StateEvent(s) 
                                                                              => possibly replies to the server with results.
                                     server replies to the client with PushStateResponse

3. Update signal state.
    client(FSU) send update request to the state server, with values to be updated to the target signals.
    state server sets the target signals with new values specified.
    state server replies to the client with results for the update.

    client sends UpdateStateRequest => server updates the SignalState(s) with new values.
                                    => server replies to the client with UpdateStateResponse.
