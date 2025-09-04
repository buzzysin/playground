package main

import (
	"testing"
)

func TestMoveVisitorPawn(t *testing.T) {
	cb := NewChessBoard()
	p := NewPawn(1, 1)
	moves := cb.GetMoves(p)
	if len(moves) != 1 || moves[0].X != 0 || moves[0].Y != 1 {
		t.Fatalf("unexpected pawn moves: %v", moves)
	}
}

func TestMoveVisitorRook(t *testing.T) {
	cb := NewChessBoard()
	r := NewRook(1, 1)
	moves := cb.GetMoves(r)
	if len(moves) < 4 {
		t.Fatalf("unexpected rook moves: %v", moves)
	}
}

func TestCaptureVisitor(t *testing.T) {
	// Use CanCapture to perform double-dispatch: attacker.CanCapture(target)
	attackerPawn := NewPawn(1, 1)
	targetPawn := NewPawn(2, 2) // pawn captures diagonally forward by 1
	if !attackerPawn.CanCapture(targetPawn) {
		t.Fatalf("pawn attacker should capture pawn target")
	}

	attackerRook := NewRook(1, 1)
	targetPawnSameRank := NewPawn(1, 3) // rook captures on same rank
	if !attackerRook.CanCapture(targetPawnSameRank) {
		t.Fatalf("rook attacker should capture pawn on same rank")
	}

	attackerKnight := NewKnight(3, 3)
	targetKnight := NewKnight(2, 5) // knight captures in L-shape (1,2)
	if !attackerKnight.CanCapture(targetKnight) {
		t.Fatalf("knight attacker should capture target knight")
	}
}
