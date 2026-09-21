#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Exercises 2",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Exercise 1
#question(title: "1.1")[
Describes the term “network edge” in your own words.
]
#answer[
Endsystems. This is where all applications which we use runs.
- Hosts
  - Clients
  - Servers
  - IoT devices
- Edge devices
  - Routers
  - Modems
]

#question(title: "1.2")[
Describes the term “access network” in your own words.]
#answer[
The access network is the entity my data runs through right before it gets to my router.

This could be *fiber cables*, *TV-cables* or *mobile net (like 5G)*
]

#question(title: "1.3")[
Describes the term “network core” in your own words.
]
#answer[
A network of networks essentially
#figure(
  image("assets/Skærmbillede 2026-09-11 kl. 12.30.16.png"),
  caption: [The Internet Topology],
)

The core main function are forwarding and routing

]

#question(title: "1.4")[
Which layer is the upper most in the current Internet protocol stack?
]
#answer[
Application layer, like HTTP
]

#question(title: "1.5")[
Describe the differences between packet and circuit switching
]
#answer[
- *Packet switching*: Packet switching allows splits teh data dynamically so it only sends data when there is something to send. (When i stop speaking in a call, it stops transmitting)
- *Circuit switching*: "The old way", allocate the entire line, whenever data is transmitted. (Even if no one is speaking in a call, its still transmitting)
]

#question(title: "1.6")[
How are ISPs interconnected?
]
#answer[
Via the internet hierarchy
- Tier 1: The Backbone (AT&T or Telia)
- Tier 2: Regional Providers (like a Danish internet provider)
- Tier 3: Local/Access ISP's (More local providers)
]

#question(title: "1.7")[
How does packet loss occur?
]
#answer[
If the buffer reaches capacity, then additional packets might be lost
]

#question(title: "1.8")[
Describe Cerf and Kahn’s internetworking principles and how they influence today’s Internet in your own words.]
#answer[
- The network-core should be simple. (no need to change the internal network to connect with others)
- Best-effort service model: The network should not promise anything about data-loss. its up to the user to detect if any data is lost.
- Stateless routing: So a router needs as little information about the data as possible (don't need to know what kind of data it is)
- Decentralized control (no master router) every one is responsible for their own router
]

= Exercise 2
Assume the network topology as shown below.
#figure(
  image("assets/Skærmbillede 2026-09-11 kl. 12.21.09.png"),
  caption: [],
)

#question(title: "2.1")[
What are the bottlenecks in this network for the connection between $(a)$ the wired client and the Internet Services, $(b)$ the wireless client and the Internet Services, $(c)$ the wired and the wireless client?
]
#answer[
The smallest link constraints end end throughput
]

#question(title: "2.2")[
What is the packet transmission delay for connection $R_1$.
]
#answer[
$ d_"trans" = L/R_1 = (10 Kbps)/(100 Mbps) = (10.000 bits)/(100.000.000 Mbps) = 10.000 sec $
]

#question(title: "2.4")[
How do the bottlenecks for $(a)$, $(b)$, and $(c)$ in 2.1 change if the home router is replaced and $R_1$ has now a bandwidth of $1000 Mbps$?
]
#answer[
$ d_"trans" = L/R_1 = (10 Kbps)/(1000 Mbps) = (10.000 bits)/(1.000.000.000 Mbps) = 100.000 sec $
]

#pagebreak()

= Exercise 3


#question(title: "3.1")[
For what does a network protocol specify rules?
]
#answer[
How to format messages and what actions should be taken when messages are received or other events take place
]
#question(title: "3.2")[
Why are network protocols organized in layers?
]
#answer[
Such that each part of the protocol is independent, this allows better identification of the data, and eases maintenance
]
#question(title: "3.3")[
Explain the term “Encapsulation” in a few sentences
]
#answer[
When encapsulation is unique for each layer, its very easily maintained since each layer only cares about 1 type of data.
]

= Exercise 4
#question(title: "4.1")[
Explain the term “Forwarding” in a few sentences.
]
#answer[
The local action, while the data is traveling. So at each input throughout the network, the router checks the "plan" made in the routing action.
]

#question(title: "4.2")[
Explain the term “Routing” in a few sentences
]
#answer[
The initial planning of the best route for the data to travel through the network.
]

= Exercise 5

#question(title: "5.1")[
Explain the term “Packet Sniffing” in a few sentences.
]
#answer[
Packets can essentially be picked up by anyone on the same network when it travels. (main defense is encryption)
]

#question(title: "5.2")[
Explain the term “IP Spoofing” in a few sentences.

]
#answer[
Spoofing is almost the opposite of sniffing, by injecting data which has been tampered with to the receiver
]


#question(title: "5.3")[
Explain the term “Denial of Service” in a few sentences.
Which categories of the STRIDE model from the Cybersecurity Fundamentals lecture does each of the three terms above correspond?
]
#answer[
When an attacker spams a serve, overloading it so resources to other become unavailable (DoS)

- Information Disclosure
- Spoofing
- Denial of Service
]