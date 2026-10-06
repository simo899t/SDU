#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 4: Diffusion",
  course: "AI508 - Comutervision",
  date: "Fall - 2026"
)

// content start here
= Evidence lower bound (ELBO)
Approximating ground truth latent variable encoder $p(z mid x)$

We denote a variational distribution with parameter $phi$ by $q_phi (z mid x)$

$ log p(x) >= EE_(q_phi (x mid x)) [log p(x,z)/(q_phi (z mid x))] $

This lowerbound will make the encoder approximate the "ground truth".

= Markovian heriarchical VAEs
Generalize VAEs with multiple levels of latent variables. General hierarchical VAEs allow connection to all previous levels.

$ p(x, z_(1:T)) = p(z_T) p_theta (x mid z_1) product^T_(t=2) p_theta (z_(t-1) mid z_t)  $
$ q(z_(1:T),x) = q_theta (z_1 mid x) product^T_(t=2) q_theta (z_t mid z_(t-1)) $

Markovian VAEs restrict to one-step transitions for both encoders and decoders

= Variational diffusion models
A markovian hierarchical VAE with the following tree characteristics: 

1. Latent dimensions $=$ data dimension
2. The encoder is parameter—free and each step simply adds Gaussian noise
3. The amount of noise added varies such that $x_T$ is standard Gaussian distributed

#figure(
  image("assets/image-1.png", width: 70%),
  caption: [One can train a decoder to learn denoising on each step $x_t$ or $x_(t-1)$],
)
#pagebreak()

= Conditional diffusion models
We rarely generate completely random images, so instead of $p(x)$ we are often interested in $p(x mid y)$. Here y is some type of conditioning information.

Examples of conditioning information:
- Text embedding
- Low-resolution image
- Image with masked parts

So make encoder steps $p_phi (x_(t-1) mid x_t)$ Conditional
$ p_phi (x_(t-1) mid x_t, y) $


= Classifier guidance
At each step $t$, classify the noise latent $x_t$ using a classifier. Classifier estimates to what degree $x_t$ conforms to conditioning information $y$

$ nabla log p(x_t mid y) = underbrace(nabla log p(x_t), "diffusion model") + gam underbrace( nabla log p(y mid x_t), "classifier") $

The classifier term adds a direction (_scaled by $gam$_) to the denoising step that increases the probability of the condition $y$, steering the sample toward $y$.

= Classifier-free guidance
Combine a conditional and an unconditional diffusion model:
$ nabla log p(x_t mid y) = gam nabla log p(x_t mid y) + (1-gam) nabla log p(x_t) $
where $lam$ control the importance attached to the conditioning
#pagebreak()

= Image generation via conditional variation \ diffusion models (VDM)
Use text embeddings as conditional information. MOst prominent is \"Stable diffusion (SD)\". Instead of embedding text alone, SD uses text$to$image embeddings (CLIP). Text embeddings are available via Unet, with cross-attention
#figure(
  image("assets/image-2.png", width: 70%),
  caption: [Example of a common CVDM \ #link("https://arxiv.org/abs/2112.10752")[https://arxiv.org/abs/2112.10752]],
)

= Inpainting via conditional variation diffusion models (VDM)
Use image as conditioning information. Parts of the images to be impainted are obstructed.

Then at each step $t$ of the denoising process, the conditioning information keeps the image grounded. 

#figure(
  image("assets/image-4.png", width: 70%),
  caption: [Inpainting examples \ #link("https://doi.org/10.1145/3528233.3530757")[https://doi.org/10.1145/3528233.3530757]],
)

