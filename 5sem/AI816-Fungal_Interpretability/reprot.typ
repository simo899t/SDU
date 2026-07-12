#import "@local/tempst:0.1.0": *
#show: project.with(
  title:         "Fungal Interpretability",
  course:        "A take on mechanistic interpretability and neuroscience",
  author:        ("Simon Holm", "Johannes Rothe"),
  date:          "Fall - 2026",
  supervisor:    "Prof. Lukas Galke Poech",
  university:    "University of Southern Denmark",
  outline:       true,
  outline-depth: 2,
  abstract: "Current methods in mechanistic interpretability struggle with faith-fulness and/or with scaling to large models. Here, we study a different approach that is inspired by mycelial networks. Fungal structures grow orthogonally through the target neural network, guided by nutrients (e.g., high activations, high gradients), branching at decision points and forming persistent connections along high-value computational pathways. The resulting hyphal network provides an adaptive map of the circuits and features that drive model behavior. Our experimental results show that [SUMMARIZE RESULTS HERE]. This paradigm naturally supports continuous monitoring: as models are fine-tuned or deployed in new contexts, fungal structures adapt to flag changes in critical circuits, offering a path toward scalable, interpretability-based oversight of large AI systems."
)

= Introduction