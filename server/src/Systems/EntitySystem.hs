module Systems.EntitySystem (updatePlayerState) where

import Core.Types (GameState(..))
import Types.Player (PlayerState(..))
-- Thêm import này để chuẩn bị cho sau này
import qualified Data.Map as Map

-- | Hàm này sẽ tìm và cập nhật trạng thái cho một người chơi cụ thể.
-- Hiện tại, logic cập nhật player đã được chuyển sang PhysicsSystem.
-- Hàm này được giữ lại làm placeholder và sẽ được sửa đổi khi cần.
updatePlayerState :: PlayerState -> GameState -> GameState
updatePlayerState _newPlayerState gs = gs