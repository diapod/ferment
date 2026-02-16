#!/usr/bin/env python

import os
import sys

from flask import Flask, request, jsonify
from mlx_lm import load, generate

MODEL = sys.argv[1] if len(sys.argv) > 1 else None
if not MODEL:
    print(f"Usage: {os.path.basename(sys.argv[0])} <model-id>", file=sys.stderr)
    raise SystemExit(2)

model, tokenizer = load(MODEL)

app = Flask(__name__)

@app.post("/generate")
def gen():
    j = request.get_json(force=True)
    prompt = j["prompt"]
    max_tokens = int(j.get("max_tokens", 512))
    temperature = float(j.get("temperature", 0.6))
    text = generate(model, tokenizer, prompt, max_tokens=max_tokens, temp=temperature)
    return jsonify({"text": text})

app.run("127.0.0.1", 8888)
