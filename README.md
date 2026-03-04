# Ferment

Ferment is an AI orchestration system for multi-model workflows.

## Training Dataset (Strict CI Mode)

Use this in CI to keep dataset exports deterministic and fail fast when build configuration drifts.

```bash
# 1) Export canonical training events from replay artifacts
bin/export-training-events \
  --in target/replay-trace.json \
  --out-events target/training/events-v1.jsonl \
  --target-format :messages

# 2) Build deterministic train/valid/test dataset (idempotent by default)
bin/build-training-dataset \
  --in target/training/events-v1.jsonl \
  --out-dir target/training/dataset \
  --target-format :messages \
  --split-seed 20260304 \
  --train-ratio 0.8 \
  --valid-ratio 0.1 \
  --test-ratio 0.1 \
  --fail-on-config-change
```

Operational notes:
- idempotency is enabled by default and skips unchanged sources,
- existing `training.event/id` records are not exported again,
- `--fail-on-config-change` stops the build when dataset config hash changes instead of doing full rebuild.

## License

This project is licensed under the Apache License 2.0 (`Apache-2.0`).

See [LICENSE](LICENSE) for the full license text.
