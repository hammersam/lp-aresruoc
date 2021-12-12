# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  def self.next_piece board
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def move_left_by_one
    @moved = move(-1, 0, 0)
  end

  def move_right_by_one
    @moved = move(1, 0, 0)
  end

  All_My_Pieces = All_Pieces.push(
    rotations([[-1, -1], [0, -1], [-1, 0], [0, 0], [1, 0]]), # phone
    [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
     [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]], # cane
    rotations([[0, -1], [0, 0], [1, 0]]) # a men leaning against on the wall
  )
end

class MyBoard < Board
  def initialize game
    super(game)
    @current_block = MyPiece.next_piece(self)
    @is_cheated = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @is_cheated
      @current_block = MyPiece.new([[[0, 0]]], self)
      @is_cheated = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...locations.size).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if score >= 100 and !@is_cheated
      @score -= 100
      @is_cheated = true
      @game.update_score
      draw
    end
  end

  def move_right_to_end
    if @game.is_running?
      ran = @current_block.move_right_by_one
      @current_pos.each{|block| block.remove}
      while ran
        ran = @current_block.move_right_by_one
      end
      draw
    end
  end

  def move_left_to_end
    if @game.is_running?
      ran = @current_block.move_left_by_one
      @current_pos.each{|block| block.remove}
      while ran
        ran = @current_block.move_left_by_one
      end
      draw
    end
  end

end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
    @root.bind('h', proc {@board.move_left}) # add vi-keybinding to tetris game :-)
    @root.bind('l', proc {@board.move_right})
    @root.bind('j', proc {@board.rotate_clockwise})
    @root.bind('k', proc {@board.rotate_counter_clockwise})
    @root.bind('H', proc {@board.move_left_to_end})
    @root.bind('L', proc {@board.move_right_to_end})
  end
end
