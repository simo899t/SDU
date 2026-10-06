#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 7: Software Defined Networking",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= Network layer
The network layer is responsible for moving datagrams (_pakker_) from a `src` host to a `dst` host across a network. 

Main functionalities are:
= Routeing
The generic router consists of 4 main components
- Input-ports
- Output-ports
- a switching fabric
- a routing processor

The routers functionality can also be split in two:
- forwarding data plane (hardware): the actual forwarding of packets 
- routing control plane (software): handles routing protocols

== Input port and forwarding
The input port receives the physical signal and works on 3 layers. The physical layer (bits), the link layer (eg. ethernet-decoding) and *the network layer*.

Decentralized switching ensures that each input-port can forward packets locally, using a forward table. 

There are multiple forwarding methods also.
- Destination based forwarding: Just look at the destination and forward in some direction using *longest prefix matching*.
- Generalized forwarding: multiple fields in the header (like IP protocol type) can optimize routing by allowing a router to perform flexible actions beyond simple forwarding, such as *dropping* packets (firewalls), *modifying headers* (NAT), or *marking* packets to signal congestion
#pagebreak()

== Switching fabrics
The switching fabrics are responsible for transferring packet from one input-port to the correct output-port.
- Switching rate: The rate to which packets are transferred. Ideally given $N$ input-ports, the switching rate should be $N times #[`linespeed`]$ .

There generally 3 primary ways to move packets.

- via memory: router acts like a pc with a cpu. `input`$to$`memory`$to$`output`. 
  #figure(
    image("assets/image-4.png", width: 70%),
    caption: [Switching via memory. packet is copied form the input port to the memory, then to the output port.],
  ) 
  This is quite limited by the memory bandwidth.
- via a bus: uses an internal bus in the router 
  #figure(
    image("assets/image-3.png", width: 70%),
    caption: [Switching via an internal bus],
  )
  This is, much like the memory, limited by the bandwidth of the bus.
#pagebreak()

- via interconnection network: One can get around the bus bottleneck by using a network of $n times n$ inter switches. 
  #figure(
    image("assets/image-5.png", width: 70%),
    caption: [$8 times 8$ multistage switch built from smaller-sized switches],
  )
  
  For larger systems (eg. Cisco CRS router), stacking these are organized into multiple switching planes.
  #figure(
    image("assets/image-6.png", width: 70%),
    caption: [Scaling, using multiple switching “planes” in parallel. \ Up to 100’s Tbps switching capacity],
  )
#pagebreak()

== Output port: Datagram Buffer and Queueing
The *output port* in a router architecture is responsible for receiving datagrams from the switch fabric and transmitting them onto the outgoing physical link. 

*Output port queuing* occurs when datagrams arrive from the router's switch fabric faster than the outgoing link's transmission rate

The *buffer* (also referred to as a queue or waiting area) is a dedicated memory space located at the input and output ports used to temporarily store incoming datagrams
#figure(
  image("assets/image-7.png", width: 70%),
  caption: [Output port queuing packets for output],
)

=== Buffer management (drop policies)
When the buffer fills up due to network congestion, arriving packets can be lost. The buffer management dictates how full buffers are handled (so we lose things we can most afford to lose)

- *Tail drop*: Discards any new arriving packet as soon as the buffer is full
- *Priority drop*: Drops or removes lower-priority packets from the buffer to make room for higher-priority traffic[2]
- *Marking*: Instead of dropping packets immediately, mark the packet sending bu filliping bit in the header. When receiver receives marked packets, they can notify the sender to reduce down sending, to avoid packet loss.

=== Packet scheduling
Packet scheduling determines the order in which queued datagrams are selected for transmission onto the link.
- *First-Come, First-Served (FCFS / FIFO)*: Transmits packets strictly in the order they arrive at the output queue.
- *Priority Scheduling*: Classifies arriving traffic into priority queues, then transmits packets from the highest-priority queue first.
- *Round Robin*: Cyclically cycles through the traffic class queues, sending one packet from each class in turn
- *Weighted Fair Queueing (WFQ)* (_Weighted Round Robin_): A generalized Round Robin policy where each class $i$ is assigned a weight $w_i$, guaranteeing a proportional share of the output link bandwidth
  $ w_i/(sum_(j) w_j) $

= Internet Protocaol (v4)

#figure(
  image("assets/image-8.png", width: 70%),
  caption: [IPv4 Datagram format],
)

== ICANN
Internet Corporation for Assigned Names and Numbers. Allocates ip addresses, through rightional registries.

CANN allocated last chunk of IPv4 addresses in 2011. IPv6 has 128-bit address space, where v4 only uses 32-bit

= DHCP client-server scenario
Initially, the client uses ip `src: 0.0.0.0` and `dst: 255.255.255.255`. Because of this, DHCP uses UDP, since every UDP packet contains a header that includes the Source Port and Destination Port.
#figure(
  image("assets/image-1.png", width: 70%),
  caption: [],
)

 

= Network Address Translation (NAT)
All devices in local network share just one IPv4 address as far as outside world is concerned. So each pc in a home share IPv4.

Then we use WAN-address ad LAN-address. NAT makes us able to us IPv4, due to the ip optimization, however, IPv6, will fix this and is better.

= IPv6

#figure(
  image("assets/image-2.png", width: 70%),
  caption: [Datagram Format for IPv6, Priority: identify priority among datagrams in flow],
)


IPv6 is only 50% implemented, so we carry it through v4 so legacy devices still work.

= VPN
Provides datagram-level encryption, authentication, integrity

Two modes
- Transport mode
  - Only datagram payload is encrypted, authenticated
- Tunnel mode
  - Entire datagram is encrypted, authenticated
  - Encrypted datagram encapsulated in new datagram with new IP header, tunneled to destination

= Internet Key Exchange (IKE)
Authentication, with either
- Pre-shared secret (PSK)
- public/private keys and certificates (PKI)