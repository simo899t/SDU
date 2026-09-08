#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 3: Networking Fundamentals",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= Computer networks
#definition(title: "Definition: Network Definition by NIST")[
  A system implemented with a collection of interconnected components. Such components may include routers, hubs, cabling, telecommunications controllers, key distribution centres, and technical control devices.

  #fill _by NIST SP 800-172_
]

= Network topology

- Connected computers 
  - Hosts: end systems
  - Network edges
- Packet switches
  - Routers and switches forward data
- Communication links
  - Any medium: copper, radio, etc.
  - Transmission rate: bandwidth

#figure(
    image("assets/image.png", width: 60%),
    caption: [Network example]
  )

#figure(
  image("assets/image-1.png"),
  caption: [The topology of a typical home network],
)

= Links and Packets
Hosts breaks messages up in 'packets' with length $L$ in bits. These are then transmitted at rate $R$
Then the 'delay' is defined as $"delay" = (L ("bits"))/(R (frac("bits","sec",style:"skewed"))$

#example(title: "Example: Shared network access: Cable")[
  #figure(
    image("assets/image-2.png"),
    caption: [Uses Hybrid Fiber Coax],
  ) <label>
  
]

#example(title: "Example: Dedicated network access: DSL")[
  #figure(
    image("assets/image-3.png"),
    caption: [Uses copper wires used in traditional landlines, signal is split],
  ) <label>
  
]

#example(title: "Example: Enterprise networks")[
  #figure(
    image("assets/image-4.png"),
    caption: [Accommodate requirements of companies, universities, etc],
  ) <label>
  
]

#example(title: "Example: Data centre networks")[
- High bandwidth links are essential
- Mostly ethernet-based connections, up to currently 400G
]

= Key functions
== Forwarding and routing
- Forwarding is a local action, moving packets from the routers input-link to the appropriate output link (_switching_)
- Routing is a global action which determines source-destination paths taken by packets
  - Different routing algorithms

== Packet switching
=== Store and forward
#figure(
  image("assets/image-5.png"),
  caption: [Takes $L/R$ seconds to transmit $L$-bit packet into link at $R$ bps],
) <label>
The entire packet must arrive at router before it can be transmitted on next link

=== Queueing
#figure(
  image("assets/image-6.png"),
  caption: [Occurs if work arrives faster than it can be served],
) <label>
If the system cannot store enough we will lose packets (data loss)

== Alternative to packet switching: Circuit switching 
#figure(
  image("assets/image-7.png"),
  caption: [Requires exclusive allocation of end-to-end ressource. Segments remain idle if bandwidth not fully used],
) <label>
- Frequency Division Multiplexing (FDM)
  - Each connection allocated its own band, can transmit at max. rate of that band
- Time Division Multiplexing (TDM)
  - Each call allocated periodic slots, can transmit at maximum rate of wider band

#example(title: "Example: Packet Switching vs Circuit Switching")[
  - Packet switching is more flexible and great for “bursty” data
      - Resource can be shared when not needed
      - Simpler, no call setup
  - Circuit Switching is better to deliver reliable data transfer
      - No packet loss due to congestion
  - Is it possible to provide circuit-like behaviour with packet switching?
    - Kinda, QoS techniques exist
]

= The Internet
On the internet, different hosts connect via ISP's. Given the number networks to interconnect. The best way to do it 
#figure(
  image("assets/image-8.png"),
  caption: [A network of networks],
)
- Tier 1 ISP which can connect everyone
- CDNs (Content delivery network) for companies like 'facebook'
- IXP (DIX in denmark) are Internet exchange point for shortcuts
- Regional ISPs
- Access ISPs

= Network Protocol
#definition(title: "Definition: Network Protocol")[
  A protocol defines the format and the order of messages sent and received among network entities, as well as the actions taken on message transmission and/or receipt.

  #fill _by J.F. Kurose & K.W. Ross, Computer Networking_
]

They specify rules for how to format messages and which actions should be taken once messages are received or other events take place
#example(title: "Example: Intuitive understanding of network protocols")[
  #figure(
    image("assets/image-9.png"),
    caption: [We do this as humans as well.],
  ) <label>  
]

Protocols are structured as layers. This eases maintenance and updating of the system

#example(title: "Definition: Air Travel")[
  #figure(
    image("assets/Skærmbillede 2026-09-08 kl. 13.21.50.png"),
    caption: [Much like air travel. We encounter much of the same protocol layers going in, as going out],
  ) <label>
]


== The layers
- *Application Layer*: Supporting Network Applications
  - HTTP, IMAP, SMTP, DNS
- *Transport Layer*: Process-process data transfer
  - TCP, UDP
- *Network Layer*: Routing of datagrams from source to destination
  - IP, routing protocols
- *Link layer*: Data transfer between neighboring network elements
  - Ethernet, 802.11 (Wifi)
- *Physical Layer*: Bits ”on the wire”

#figure(
  image("assets/image-10.png"),
  caption: [Encapsulation - an end-to-end example],
) <label>


Data is 'encapsulated' through each layer. At each source layer data is wrapped in a 'head' which can be unwrapped again by the destination.

#example(title: "The ISO/OSI reference model")[
  Some books/internet sources might reference the "ISO/OSI" reference model. However we have since its creation removed teh presentation adb session layer.
  
  #align(center)[#text(red)[#strong[WILL NOT BE IN THE EXAM]]]
  
]

= The four sources of Packet Delay
#figure(
  image("assets/image-12.png"),
  caption: [],
) <label>

#example(title: "Example: A car analogy")[
  Given 10 cars (aka 10-bit packet) where the toll takes 12 sec and propagation of 1 hour:

  #figure(
    image("assets/image-13.png"),
    caption: [full travel will take 62 minutes],
  ) <label>
  
]

See this using 
```terminal
traceroute google.com
```
= Throughput
#figure(
  image("assets/image-14.png"),
  caption: [Bottleneck link: The smallest link constraints end-end throughput],
) <label>
