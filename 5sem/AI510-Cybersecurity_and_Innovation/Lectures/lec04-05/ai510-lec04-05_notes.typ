#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 4 & 5: Networks - Application Layer",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= Creating Network Applications
Networks are made on a lower layer such that when creating application, we dent have to create software for the network.
- The server (host) is always available, with a permanent ip address
- clients contact the host
- may be intermediate
- ip may be dynamic

== Peer to peer architecture
Alternative server method, which is not always on.
- System instead communicate directly

== Process communicating
- The host has a program that is running
  - Within the host there is inter-process communication (OS)


== Sockets
Interface to the lower layers within a system. To receive message we need to identify ourselves, using an IP *address* and a *port*

= Application layer protocols
- Types of messages received: request and responses
- Message syntax
- Message semantics
- Rules
- Open protocols
- Proprietary protocols

== What it needs to provide
- *Data integrity*:
  - reliable data transfer, though some specific apps might tolerate some data loss (audio)
- *Timing*:
  - low delay (like video/call chat or games)
- *Throughput*:
  - Consistent throughput (streaming)

= HTTP - hypertext transfer protocol
- HTTP uses TCP to exchange messages between browser (HTTP client) to server (HTTP host)
  - This is "stateless" in the sense that it doest keep information about past clients.

== Non-persistent HTTP
1. client initiates connection on port 80
2. server accepts
3. client requests files
4. server closes TCP connection
5. client receives message (html file)
- Round-trip time (RTT)
  - requires 2 RTT per. object #emoji.face.sleep
Status line (status code)
== Persistent HTTP
is better

== HTTP messages
Theres two types of `HTTP` messages: request and response
- requests
  - `GET`: sends data to server in URL using ? and &
  - `POST`: sends data to server in message body. (eg. form input)
  - `HEAD`: requests header (only) that would be returned if specified URL were requested with a `HTTP GET` method
- responses
  - status codes:
    - 200 OK
    - 301 Moved Permanently
    - 302 Moved Temp
    - 400 Bad request
    - 403 Forbidden
    - 404 Not found
    - 500 Error
    - 418 ... #emoji.skull

== Cookies
Cookies help maintaining state from HTTP (like remembering a shopping cart on a webshop)
They can be used for:
- Authorization
- Shopping carts
- Recommendations
- User session states (web e-mail)

Cookies are sent via http, which means that the website does not need to keep your "state". 

_Note: cookies permit sites to learn a lot about you on their site_

=== Cookies for ads
#figure(
  image("assets/image-2.png"),
  caption: [],
) <label>

=== Caches in HTTP performance
Instead of everyone at your place accessing the same origin server. One can just use caches on the home server.

#figure(
  image("assets/image-3.png"),
  caption: [],
) <label>

In the same sense. If you have just seen a website, just save the resources in a cache and load them next time you visit (if its unmodified).

== Email protocols
Mail serves only have: A mailbox and a message queue

=== Simple Mail Transfer Protocol
Uses *TCP* to reliably transfer email message from client. 
#figure(
  image("assets/image-4.png"),
  caption: [],
) <label>
_Note: This is only for *sending* mails_

== SMTP vs HTTP
  - HTTP
- Client Pull
- ASCII commands
- ASCII status codes
- Each object encapsulated in its own response message
- Non-persistent & persistent connections
  - SMTP
- Client Push
- ASCII commands
- ASCII status codes
- Multiple objects sent in multipart message
- Persistent connections

== Retrieving mails: Mail access protocols
#figure(
  image("assets/image.png"),
  caption: [fig],
) <label>

IMAP: Internet Mail Access Protocol [RFC 3501]: messages stored on server, IMAP provides retrieval, deletion, folders of stored messages on server

_or use POP_ 

= Securing E-mail
- Confidentiality of message content
- Authenticity of sender

== Option 1: Transport Layer Security
Assumptions:
- Clients and servers trusted
- Network untrusted

Protocols encapsulted in secure connections on transport layer (TLS)
- Advantage: easier, since it only needs to be deployed by servers
- Disadvantage: servers must be trusted

== Option 2: End-to-end Security
Assumptions:
- Clients trusted
- Network and servers untrusted

Message body encrypted/decrypted & signed/verified by user agents
- Advantage: Strictly more secure
- Disadvantage: More complicated for users

== OpenPGP
Technically the older standard, due to predecessor PGP (1991)

#figure(
  image("assets/image-1.png"),
  caption: [],
)

= DNS
Allows you to find the right host to contact. (you might have some info on someone, but not their ip address)

- DNS is a distributed database to map between IP address and name, and vice versa

- DNS is a hierarchical database
#figure(
  image("assets/image-5.png"),
  caption: [],
)

DNS information can also be cached

== Records
- `type=A/AAAA`
  - `name` is hostname
  - `value` is ip address
- `type=NS`
  - `name` is domain (like cogito.dk)
  - `value` is hostname of authoritative name server for this domain
- `type=A/AAAA`
  - `name` is s alias name for some “canonical” (the real) name
  - `value` is canonical name
- `type=A/AAAA`
  - `value` is name of SMTP mail server associated with name

== Security
- DDos attacks by redirecting long requests to a spoofed IP
- Spoofing attacks, intercept DNS queries. 

== DNS against phishing
=== Spoofing
Assume email sender spoofing like shown below
#figure(
  image("assets/image-6.png"),
  caption: [Alice might not be who she is saying she it.],
)

We can use dns to confirm ip is correct.
#figure(
  image("assets/Skærmbillede 2026-09-15 kl. 13.34.59.png"),
  caption: [Spoofer is caught using DNS],
)

=== Message Manipulation
Assume that someone might intercept your message, and corrupt it
#figure(
  image("assets/image-8.png"),
  caption: [],
)
#figure(
  image("assets/Skærmbillede 2026-09-15 kl. 13.38.29.png"),
  caption: [],
)

= Next Lecture
- TCP (transmission control protocol)
  - Provides control in transport and flow
  - Does not provide good timing
- UCP
  - Provides good timing and security with little control



