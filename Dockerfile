FROM rust:1.83-bookworm AS builder
WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src ./src
COPY build.rs ./
RUN cargo build --release

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates git \
    && rm -rf /var/lib/apt/lists/*
COPY --from=builder /app/target/release/pascal /usr/local/bin/pascal
WORKDIR /workspace
ENTRYPOINT ["pascal"]
CMD ["--help"]
