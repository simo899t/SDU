#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Exercises 3",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Exercise 1
#question(title: "1.1")[
WHat are sockets used for?
]
#answer[
Sockets is the \"door\" (or interface) which processes send and receive messages to and from. 
]

#question(title: "1.2")[
What does HTTP stand for and what is the protocol used for?
]
#answer[
HTTP stands for Hypertext Transport Protocol and is the application layer protocol of the internet. HTTP is used to transfer data between web browsers and web users on the internet.
]

#question(title: "1.3")[
Is the following statement true for HTTP?
$ "\"HTTP is stateless by default, but statefulness can be enabled by the client\"" $
Give a brief explanation of your answer.
]
#answer[
No. HTTP is stateless by design in the fact that no information is kept via http, so each request is an isolated event. However clients (like browsers) can choose to keep information (like cookies). This does not make HTTP stateful in any way.
]

#question(title: "1.4")[
What is the difference between persistent and non-persistent HTTP?
]
#answer[
Non-Persistent HTTP has 2 RTT (open, connect, retrieve-respond, close) for each objects it sends. To overcome this browsers will often open multiple TCP connections for faster fetching, by keeping connection open after responses for more files to be received and sent.
]

#question(title: "1.5")[
What is the difference between the two HTTP status codes 403 and 404?
]
#answer[
- *403 - FORBIDDEN*: Clients identity is known to server and is forbidden access. 
- *404 - NOT FOUND*: The server did not find the requested resource.
]
#pagebreak()

#question(title: "1.6")[
Why was HTTP/2 introduced?
]
#answer[
If users requests 1 lager object before 3 smaller objects, they will have to wait for the big object. The goal was to decrease this delay in multi-object HTTP requests.
#figure(
  image("assets/image.png"),
  caption: [objects delivered in order requested: $O_2,O_3,O_4$ wait behind $O_1$],
)

HTTP/2 divides objects into frames, so transmission can be more dynamic.
#figure(
  image("assets/image-1.png"),
  caption: [$O_2,O_3,O_4$ delivered quickly, $O_1$ was slightly delayed],
)
]

#question(title: "1.7")[
What is the purpose of web caches?
]
#answer[
Web caches saves unchanged versions of a response on a users device, so they don't have to request it again (if unchanged)
]
#pagebreak()

#question(title: "1.8")[
Is it true that SMTP is used to send, receive, and access emails? Give a brief explanation of your answer.
]
#answer[
No, SMTP (Simple Mail Transfer Protocol) is delivery/storage of e-mail messages to receiver’s server. One would need something like IMAP (Internet Mail Access Protocol) for receiving as well.
]

#question(title: "1.9")[
What is the difference between the approaches to trusting keys in OpenPGP and S/MIME?
]
#answer[
Main difference is \"the approver\". The OpenPGP is based on the internet of trust (like friends of friends), while the S/MIME has a centralized authority (which you can choose to trust). 
]

= Exercise 2
#question(title: "2.1")[
Explain the term \“Client-Server Paradigm\”
]
#answer[
Then servers act as a permanent host (like data centres) and clients communicate with the server. 
]

#question(title: "2.2")[
Explain the term \“Peer-to-peer Architecture\”
]
#answer[
No permanent host server

Peers request service from other peers, provide service in return to other peers
]

#question(title: "2.3")[
Explain the term “elastic” in the context of network throughput.
]
#answer[
Elastic throughput means that an application can function with a throughput that drastically varies.
]

#question(title: "2.4")[
Explain the difference between iterated and recursive queries in DNS.
]
#answer[
When a dns server needs to find an ip address, it can either (recursively) find the actual ip by traveling other dns servers. or (iteratively) reference the next dns server.
]

= Exercise 3
In this exercise you will make first experiences with capturing and analysing network data.

#question(title: "3.0")[
Download & install Wireshark on your laptop/PC from: #link("https://www.wireshark.org/download.html")[https://www.wireshark.org/download.html]. Then, familiarize yourself with the Wireshark UI and how to capture live network data using chapters
3&4 of its documentation: #link("https://www.wireshark.org/docs/wsug_html_chunked/index.html")[https://www.wireshark.org/docs/wsug_html_chunked/index.html]
]

#question(title: "3.1")[
Start a live network data capture. While running the live capture, visit the website #link("http://example.com")[http://example.com] in your browser. Then wait a few moments and reload the page in your browser. Then stop the capture. By looking at the captured data, answer the following questions:
+ How long did it take for the server to respond to the first HTTP GET with an HTTP OK?
+ What is the IP address of your computer?
+ What is the IP address of the example.com server?
+ Is your browser running HTTP version 1.0, 1.1, or 2? What version of HTTP is the server running?
+ What is the HTTP status code returned by the server in the first HTTP GET request?
+ How many bytes of content are being returned to your browser in the response to the first HTTP GET request?
+ How long is the data that was sent to the browser as response to the first HTTP GET request valid if it is cached? Which header field tells you this info?
+ How does the HTTP GET request to the server change when reloading the page? How does
the server’s response change from the original response?
+ Find the first DNS query involving the domain example.com. To which IP address was the query sent?
+ What type is specified in the DNS query?
+ What is the answer data in the response to the query?
]

#pagebreak()

#answer[
  Due to my browser redirecting to https, i use #link("http://www.testingmcafeesites.com/testcat_al.html")[http://www.testingmcafeesites.com/testcat_al.html] instead.
#figure(
  image("assets/image-2.png"),
  caption: [wireshark output for #link("http://www.testingmcafeesites.com/testcat_al.html")[http://www.testingmcafeesites.com/testcat_al.html]],
)

+ It took `0.18679` seconds
+ `10.126.38.175`
+ `100.21.215.181`
+ HTTP/1.1 
+ `200`
+ `370` bytes
+ `Length`, `370` bytes
+ Because of my browser, no change
+ 
+ 
]



