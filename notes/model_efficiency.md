Prefix caching cuts prefill from 22.2B to 4.7B tokens, a 79% saving, so it's the biggest lever you have.
1. **Put the state first and the abstract last.** llama-server's `cache_prompt` and vLLM's `--enable-prefix-caching` then reuse the prefix.
2. **Force a tiny output:**
   - `temperature=0` and `max_tokens` of 1–3.
   - A grammar or guided choice.
   - Or read the logprobs of the label tokens and skip generation.
3. **Disable thinking.**
4. **Keep everything on the GPU:** `-ngl 99`, `-fa on`, `-c` around 4096 per slot, not 131k. Run several slots (`-np`) with continuous batching, `-b/-ub` 1024–2048, and a q8_0 KV cache.
5. **Use Q4_K_M or Q5.** Classification tolerates these well.
6. **Cascade or distill:**
   - Have a large model label 2–5k abstracts.
   - Check the small model's agreement against those labels, especially recall.
   - Route low-margin cases to a bigger model.
   - Or fine-tune an encoder such as BiomedBERT on the labels. It would process 10M abstracts in hours, not weeks.

