# messagepack-rpc

[![Build Status](https://travis-ci.org/rodrigosetti/messagepack-rpc.svg?branch=master)](https://travis-ci.org/rodrigosetti/messagepack-rpc)

[Message Pack](http://msgpack.org) RPC over TCP.

Right now this implementation only supports TCP, but the plan is to support multiple transports ( UDP, UNIX domain sockets, _etc._)

## MessagePack-RPC Protocol specification

Reference: http://wiki.msgpack.org/display/MSGPACK/RPC+specification

The protocol consists of "Request" message and the corresponding "Response" message. The server must send "Response" message in reply with the "Request" message.

### Request Message

The request message is a four elements array shown below, packed by MessagePack format.

    [type, msgid, method, params]

#### type

Must be zero (integer). Zero means that this message is the "Request" message.

#### msgid

The 32-bit unsigned integer number. This number is used as a sequence number. The server replies with a requested msgid.

#### method

The string, which represents the method name.

#### params

The array of the function arguments. The elements of this array is arbitrary object.

### Response Message

The response message is a four elements array shown below, packed by MessagePack format.

    [type, msgid, error, result]

#### type

Must be one (integer). One means that this message is the "Response" message.

#### msgid

The 32-bit unsigned integer number. This corresponds to the request message.

#### error

If the method is executed correctly, this field is Nil. If the error occurred at the server-side, then this field is an arbitrary object which represents the error.

#### result

An arbitrary object, which represents the returned result of the function. If error occurred, this field should be nil.

### Notification Message (not yet supported)

The notification message is a three elements array shown below, packed by MessagePack format.

    [type, method, params]
    
#### type

Must be two (integer). Two means that this message is the "Notification" message.

#### method

The string, which represents the method name.

#### params

The array of the function arguments. The elements of this array is arbitrary object.

### The Order of the Response

The server implementations don't need to send the reply, in the order of the received requests. If they receive the multiple messages, they can reply in random order.

This is required for the pipelining. At the server side, some functions are fast, and some are not. If the server must reply with in order, the slow functions delay the other replies even if it's execution is already completed.
