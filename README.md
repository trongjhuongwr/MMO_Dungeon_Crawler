# MMO Tank Dungeon

`MMO Tank Dungeon` là một dự án trò chơi đa người chơi (multiplayer) thuộc thể loại Bắn súng-Hành động góc nhìn từ trên xuống (Top-down Shooter). Dự án được triển khai hoàn toàn bằng ngôn ngữ lập trình hàm Haskell, thể hiện một kiến trúc client-server hiệu năng cao với logic trạng thái được quản lý chặt chẽ.

Người chơi điều khiển một cỗ xe tăng, chiến đấu trong các đấu trường hoặc hầm ngục, hỗ trợ cả hai chế độ chơi PvP (Đối kháng) và PvE (Chống lại AI).

## 1\. Kiến trúc Hệ thống (System Architecture)

Dự án được cấu trúc dưới dạng "monorepo" được quản lý bởi `stack`, bao gồm ba gói (package) chính: `client`, `server`, và `shared`.

### 1.1. Máy chủ (Server)

Máy chủ được thiết kế theo mô hình **Máy chủ có thẩm quyền (Authoritative Server)**. Toàn bộ logic cốt lõi của trò chơi được xử lý và xác thực tại đây, bao gồm:

  * **Vòng lặp Trò chơi (Game Loop):** Một vòng lặp `tickRate` cố định (ví dụ: 30 tick/giây) chịu trách nhiệm thực thi các hệ thống con theo thứ tự: AI, Vật lý, Chiến đấu, và gửi `WorldSnapshot` đến người chơi.
  * **Mạng (Networking):** Sử dụng mô hình mạng lai (hybrid network model).
      * **TCP (`network`):** Xử lý các giao tiếp cần độ tin cậy cao và đúng thứ tự, bao gồm: xác thực đăng nhập/đăng ký, quản lý sảnh chờ (lobby), và các sự kiện bắt đầu/kết thúc trận đấu.
      * **UDP (`network`):** Xử lý các gói tin thời gian thực, có thể chấp nhận mất mát dữ liệu (lossy), bao gồm: nhận lệnh điều khiển (`PlayerCommand`) từ client và phát `WorldSnapshot` (trạng thái thế giới) đến client.
  * **Đồng thời (Concurrency):** Hệ thống sử dụng các luồng nhẹ (lightweight threads) của Haskell (`forkIO`) để quản lý đồng thời nhiều kết nối client và nhiều phòng chơi (rooms).
  * **Đồng bộ hóa (Synchronization):** Trạng thái toàn cục (`ServerState`) và trạng thái phòng (`RoomGameState`) được bảo vệ khỏi các truy cập đồng thời (race conditions) thông qua `MVar`.

### 1.2. Máy khách (Client)

Máy khách được thiết kế theo mô hình **Máy khách mỏng (Thin Client)**.

  * **Hiển thị (Rendering):** Sử dụng thư viện `gloss` để thực hiện render 2D. Vòng lặp `playIO` của Gloss chịu trách nhiệm chính cho việc cập nhật và hiển thị.
  * **Logic:** Client hầu như không chứa logic trò chơi. Nhiệm vụ chính là thu thập dữ liệu đầu vào (phím, chuột), gửi chúng đến server, và hiển thị `WorldSnapshot` nhận được từ server.
  * **Quản lý Tài nguyên:** Sử dụng `gloss-juicy` để tải và quản lý các tài nguyên hình ảnh (textures).

### 1.3. Gói Chia sẻ (Shared)

Đây là thư viện cốt lõi, đảm bảo tính nhất quán về kiểu dữ liệu giữa server và client. Nó định nghĩa:

  * **Giao thức Mạng (Network Protocol):** Định nghĩa tất cả các gói tin (packets) TCP và UDP (`ClientTcpPacket`, `ServerUdpPacket`, v.v.).
  * **Kiểu dữ liệu (Data Types):** Định nghĩa các cấu trúc dữ liệu cơ bản của trò chơi như `PlayerState`, `BulletState`, `TankType`, và `GameMap`.

-----

## 2\. Công nghệ Nổi bật

| Thành phần | Công nghệ | Mục đích |
| :--- | :--- | :--- |
| **Ngôn ngữ** | **Haskell** (GHC) | Ngôn ngữ lập trình chính cho toàn bộ dự án. |
| **Build** | **Stack** | Quản lý phụ thuộc và biên dịch dự án (bao gồm cả ba gói). |
| **Mạng** | **`network`** | Cung cấp API Socket cấp thấp cho cả TCP và UDP. |
| **Đồ họa** | **`gloss`** | Thư viện render 2D cho máy khách. |
| **Tải Asset** | **`gloss-juicy`** | Tải các định dạng ảnh (ví dụ: PNG) cho `gloss`. |
| **Concurrency** | `MVar`, `forkIO` | Quản lý trạng thái chia sẻ và chạy đồng thời các tiến trình (network, game loop). |
| **Parallelism** | RTS Flags (`-threaded`, `-N`) | Cho phép GHC Runtime System thực thi các luồng Haskell trên nhiều lõi CPU vật lý. |
| **Database** | **`sqlite-simple`** | Cung cấp cơ sở dữ liệu SQLite cục bộ (`mmo_server.db`). |
| **Xác thực** | **`bcrypt`** | Băm và xác thực mật khẩu người dùng một cách an toàn. |
| **Cấu hình** | **`yaml`** | Đọc cấu hình máy chủ (`server.yaml`) và máy khách (`client.yaml`). |
| **Bản đồ** | **`aeson`** | Phân tích (parse) các tệp bản đồ định dạng `.json`. |
| **Serialization** | **`binary`** | Mã hóa và giải mã các gói tin mạng (network packets). |

## 3\. Các Tính năng Cốt lõi (Đã triển khai)

  * **Xác thực Người dùng:** Hỗ trợ đăng ký và đăng nhập tài khoản. Mật khẩu được lưu trữ an toàn bằng `bcrypt`.
  * **Hệ thống Sảnh chờ (Lobby):** Người chơi có thể tạo phòng (`CTP_CreateRoom`), tham gia phòng (`CTP_JoinRoom`), chọn loại Tank (`Rapid` hoặc `Blast`), và báo "Sẵn sàng" (`CTP_UpdateLobbyState`).
  * **Chế độ chơi PvP:** Hỗ trợ trận đấu 1v1. Trận đấu bắt đầu khi cả hai người chơi đều sẵn sàng.
  * **Chế độ chơi PvE:** Hỗ trợ trận đấu 1vAI. Người chơi chọn tank cho mình và cho Bot (`CTP_StartPvEBotMatch`).
  * **Hệ thống AI:** Bot trong chế độ PvE có logic `AISystem` riêng, bao gồm trạng thái tuần tra (patrolling) và trạng thái chiến đấu (engaging) với logic bắn theo loạt (burst fire).
  * **Hệ thống Chiến đấu:**
      * Hỗ trợ hai loại xe tăng (`Rapid`, `Blast`) với các chỉ số khác nhau (tốc độ bắn, sát thương, tốc độ di chuyển).
      * Đạn được mô phỏng vật lý, có thời gian sống (lifetime) và bị hủy khi va chạm.
      * Hỗ trợ hệ thống mạng sống (lives) và hồi sinh (respawn).
  * **Giao diện Người dùng (UI):**
      * Client có hệ thống máy trạng thái (state machine) để quản lý các màn hình: Login, Menu, Lobby, InGame, PostGame, Paused.
      * Trong game, HUD hiển thị thanh máu, số mạng sống.
      * Radar hiển thị vị trí tương đối của đối thủ.
  * **Vòng lặp Trận đấu:** Hỗ trợ trạng thái kết thúc (GameOver) và chức năng đấu lại (Rematch).

## 4\. Cấu trúc Thư mục

```
MMO_Dungeon_Crawler/
├── client/
│   ├── assets/         (Tài nguyên đồ họa, bản đồ, âm thanh)
│   │   ├── maps/       (*.json)
│   │   └── textures/   (*.png)
│   ├── config/         (client.yaml)
│   ├── src/
│   │   ├── Core/       (Renderer, Animation, Effect)
│   │   ├── Network/    (Client.hs)
│   │   ├── UI/         (HUD.hs, Screens.hs)
│   │   ├── Main.hs     (Entry point của Client)
│   │   ├── Types.hs    (Định nghĩa máy trạng thái client)
│   │   └── ...
│   ├── package.yaml    (Phụ thuộc của Client)
│   └── test/           (InputSpec.hs)
│
├── server/
│   ├── app/
│   │   ├── Main.hs     (Entry point của Server)
│   │   ├── ServerApp.hs (Khởi tạo server)
│   │   └── CLI.hs      (Hỗ trợ các lệnh command-line)
│   ├── assets/
│   │   └── maps/       (pvp.json, dungeon_level_1.json)
│   ├── config/         (server.yaml)
│   ├── db/             (Schema mẫu)
│   ├── migrations/     (SQL migration)
│   ├── src/
│   │   ├── Core/       (Types.hs, Config.hs)
│   │   ├── Data/       (Database.hs, PlayerQuery.hs)
│   │   ├── Network/    (TCPServer.hs, UDPServer.hs)
│   │   ├── Systems/    (AISystem, CombatSystem, PhysicsSystem, MapLoader)
│   │   ├── Utils/      (Random.hs)
│   │   └── GameLoop.hs (Logic vòng lặp game chính)
│   └── package.yaml    (Phụ thuộc của Server)
│
├── shared/
│   ├── src/
│   │   ├── Network/    (Packet.hs)
│   │   └── Types/      (Player.hs, Bullet.hs, Tank.hs, Map.hs, GameMode.hs, etc.)
│   └── package.yaml    (Phụ thuộc của Shared)
│
├── .gitignore
├── Gameplay.md
├── Pipeline.md
├── README.md (Tệp này)
└── stack.yaml (File quản lý dự án chính)
```

## 5\. Thiết lập và Khởi chạy

Dự án được xây dựng bằng công cụ `stack`.

### 5.1. Yêu cầu hệ thống

  * [Stack (The Haskell Tool Stack)](https://docs.haskellstack.org/en/stable/)
  * Bộ giải quyết (resolver) LTS: `lts-22.7` (GHC 9.6.5)

### 5.2. Biên dịch

Biên dịch toàn bộ dự án (server, client, và shared):

```bash
stack build
```

### 5.3. Khởi chạy Máy chủ

Máy chủ sẽ được biên dịch và đặt tại `.stack-work/install/.../bin/mmo-server`.

```bash
stack exec mmo-server
```

Máy chủ sẽ khởi động, kết nối tới cơ sở dữ liệu `mmo_server.db`, và bắt đầu lắng nghe trên cả hai cổng TCP và UDP được định nghĩa trong `server/config/server.yaml`.

### 5.4. Khởi chạy Máy khách

Máy khách sẽ được biên dịch và đặt tại `.stack-work/install/.../bin/mmo-client`.

```bash
stack exec mmo-client
```

Máy khách sẽ mở một cửa sổ đồ họa (`gloss`) và tự động kết nối đến địa chỉ máy chủ được định nghĩa trong `client/config/client.yaml`.
