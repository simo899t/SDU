#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 2: Vision Transformer",
  course: "AI508 - Comutervision",
  date: "Fall - 2026"
)

// content start here
= Tokenizations
#figure(
  image("figures/figure-1.svg"),
  caption: [Tokenize by splitting the image up in patches. Note that these are commonly split into $16 times 16$],
) <label>

= Embedding

= Swin transformer
- Window-based self-attention (W-MSA) 
  
  Uses patch merging: Same idea as in pooling, but instead merge patches, which attend to each other most so instead of attending globally, the image is split (by patch merging) into non-overlapping windows (e.g. 7×7 patches), and self-attention is computed only within each window. This makes more efficient complexity

- Shifted windows
  
  Window attention alone means patches in different windows never interact. Swin alternates between two layouts across consecutive blocks: regular windows, then windows shifted. This shift creates cross-window connections in the next layer without the cost of global attention.
  
= ViT code
Huggingface ViT #link("https://huggingface.co/docs/transformers/model_doc/vit")[*[link]*]

Model github #link("https://github.com/huggingface/transformers/blob/v5.17.0/src/transformers/models/vit/modeling_vit.py")[*[link]*]