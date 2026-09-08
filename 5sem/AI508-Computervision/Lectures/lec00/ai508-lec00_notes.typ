#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 0: Course Introduction",
  course: "AI508 - Comutervision",
  date: "Fall - 2026"
)

// content start here
= About comutervision

#definition(title: "Definition: Comutervision")[
  The study and practice of learning anf computing with visual data to infer, organize, and synthesize information about the visual world.
]

#definition(title: "Definition: Specific definition")[
  The study and practice of developing and applying deep learning models for perceiving and generating visual and multi modal data.
]

Note that visual data refers to images, videos and other 2d/3d signals

== Subfields
- Image processing (de-noising, de-blurring, superresolution, color)
- Low level vision (edge/line/corner/feature detection)
- Geometry (stereo vision, depth/pose estimation, multi-view geometries)
- *Perception* (object detection, classification, segmentation)
- *Synthesis* (image/video generation, in/out-painting, style transfer)
- *Multimodal* (contrastive language-image pre-training, vision language models)

for this course, *these* are mostly relevant.

= Lectures
- Lecture 0 Introduction (this)
- Lecture 1 YOLO (you only look once)
- Lecture 2 Vision transformer
- Lecture 3 Diffusion
- Lecture 4 Vision language models
- Lecture 5 Segment anything