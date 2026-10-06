#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 7: Software Defined Networking",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= Generalized forwarding
Many header fields can determine action. Many actions are possible, like `drop`/`copy`/`modify`/`log` packet

#figure(
  image("assets/image-1.png", width: 70%),
  caption: [Generalized forwarding using flow-tables which just mach ips with an action],
)

This match + action allows implementation of various functions like
- Router
- NAT
- Switch
- Firewall

== Openflow

#figure(
  image("assets/image-2.png", width: 70%),
  caption: [Example of a standard for flow table],
)

= Software Defined Networking
SDN help with multiple problems
- specialized routing
- access Control
- load balance
== Control plane

== Stack

=== SDN Controller

= OpenFlow protocol
-  Controller-to-switch
-  Switch-to-controller




