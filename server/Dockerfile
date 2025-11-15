# === BUILDER ===
FROM fpco/stack-build:lts-22.7 AS builder
WORKDIR /app

# SAO CHÉP CÁC TỆP CẤU HÌNH
COPY stack.yaml ./
COPY stack.yaml.lock ./
COPY server/package.yaml ./server/
COPY server/server.cabal ./server/
COPY client/package.yaml ./client/
COPY client/client.cabal ./client/
COPY shared/package.yaml ./shared/
COPY shared/shared.cabal ./shared/

# CÀI ĐẶT GHC
RUN stack setup

# SAO CHÉP TOÀN BỘ SOURCE CODE
COPY . .

# BUILD TẤT CẢ (Dependencies + Code)
RUN stack build server:exe:mmo-server --copy-bins --local-bin-path /app/bin


# === FINAL ===
FROM debian:stable-slim AS final

# Cài đặt thư viện hệ thống
RUN apt-get update && apt-get install -y \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy file thực thi đã build
COPY --from=builder /app/bin/mmo-server .

# Copy các tệp cấu hình, assets, migrations
COPY server/config/server.yaml ./server/config/
COPY server/assets/ ./server/assets/
COPY server/migrations/ ./server/migrations/

# Mở các port
EXPOSE 4000/tcp
EXPOSE 8888/udp

# Lệnh chạy
CMD ["/app/mmo-server"]