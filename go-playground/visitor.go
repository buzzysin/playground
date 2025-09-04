package main

// Simple Visitor pattern demo: Move and Capture visitors for chess pieces.

// Move is a simple 2D move vector.
type Move struct{ X, Y int }

// ChessBoard holds pieces and provides helper methods.
type ChessBoard struct {
	pieces []ChessPiece
}

func NewChessBoard() *ChessBoard             { return &ChessBoard{} }
func (cb *ChessBoard) AddPiece(p ChessPiece) { cb.pieces = append(cb.pieces, p) }

// ChessPiece is the common interface for pieces. Matches UML: getRank, getMoves(board), canCapture(other).
type ChessPiece interface {
	Rank() int
	GetMoves(*ChessBoard) []Move
	CanCapture(ChessPiece) bool
	AcceptCapture(CaptureVisitorInterface) bool
	File() int
	Type() PieceType
}

// PieceType is a simple enum for piece kinds.
type PieceType int

const (
	PawnType PieceType = iota
	RookType
	KnightType
)

// MoveVisitorInterface defines visit methods that return a list of moves. Visitor may need board context.
type MoveVisitorInterface interface {
	VisitPawn(*Pawn, *ChessBoard) []Move
	VisitRook(*Rook, *ChessBoard) []Move
	VisitKnight(*Knight, *ChessBoard) []Move
}

// CaptureVisitorInterface defines visit methods where the visitor carries the attacker and the visited is the target.
type CaptureVisitorInterface interface {
	VisitPawn(*Pawn) bool
	VisitRook(*Rook) bool
	VisitKnight(*Knight) bool
}

// Concrete MoveVisitor which may use board context.
type MoveVisitor struct{ board *ChessBoard }

func (mv *MoveVisitor) VisitPawn(p *Pawn, b *ChessBoard) []Move {
	// Pawn: one forward move (demo only)
	return []Move{{0, 1}}
}

func (mv *MoveVisitor) VisitRook(r *Rook, b *ChessBoard) []Move {
	// Rook: four orthogonal sample moves
	return []Move{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
}

func (mv *MoveVisitor) VisitKnight(k *Knight, b *ChessBoard) []Move {
	// Knight: a couple of L-shaped moves (demo)
	return []Move{{1, 2}, {2, 1}, {-1, 2}, {-2, 1}}
}

// CaptureVisitor carries the attacker; visited piece accepts it and the visitor decides if attacker can capture the visited.
type CaptureVisitor struct{ attacker ChessPiece }

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (cv *CaptureVisitor) VisitPawn(target *Pawn) bool {
	switch a := cv.attacker.(type) {
	case *Pawn:
		// Pawn captures diagonally forward by 1 (demo): rank+1 and file diff 1
		return a.Rank()+1 == target.Rank() && abs(a.File()-target.File()) == 1
	case *Rook:
		// Rook captures on same rank OR same file
		return a.Rank() == target.Rank() || a.File() == target.File()
	case *Knight:
		// Knight captures in L-shape: (1,2) or (2,1)
		rd := abs(a.Rank() - target.Rank())
		fd := abs(a.File() - target.File())
		return (rd == 1 && fd == 2) || (rd == 2 && fd == 1)
	default:
		return false
	}
}

func (cv *CaptureVisitor) VisitRook(target *Rook) bool {
	switch a := cv.attacker.(type) {
	case *Pawn:
		return a.Rank()+1 == target.Rank() && abs(a.File()-target.File()) == 1
	case *Rook:
		return a.Rank() == target.Rank() || a.File() == target.File()
	case *Knight:
		rd := abs(a.Rank() - target.Rank())
		fd := abs(a.File() - target.File())
		return (rd == 1 && fd == 2) || (rd == 2 && fd == 1)
	default:
		return false
	}
}

func (cv *CaptureVisitor) VisitKnight(target *Knight) bool {
	switch a := cv.attacker.(type) {
	case *Pawn:
		return a.Rank()+1 == target.Rank() && abs(a.File()-target.File()) == 1
	case *Rook:
		return a.Rank() == target.Rank() || a.File() == target.File()
	case *Knight:
		rd := abs(a.Rank() - target.Rank())
		fd := abs(a.File() - target.File())
		return (rd == 1 && fd == 2) || (rd == 2 && fd == 1)
	default:
		return false
	}
}

// Pawn piece
type Pawn struct {
	rank, file int
	ptype      PieceType
}

func NewPawn(rank int, file int) *Pawn                                  { return &Pawn{rank: rank, file: file, ptype: PawnType} }
func (p *Pawn) Rank() int                                               { return p.rank }
func (p *Pawn) File() int                                               { return p.file }
func (p *Pawn) Type() PieceType                                         { return p.ptype }
func (p *Pawn) AcceptMove(v MoveVisitorInterface, b *ChessBoard) []Move { return v.VisitPawn(p, b) }
func (p *Pawn) AcceptCapture(v CaptureVisitorInterface) bool            { return v.VisitPawn(p) }
func (p *Pawn) GetMoves(b *ChessBoard) []Move {
	mv := &MoveVisitor{board: b}
	return p.AcceptMove(mv, b)
}
func (p *Pawn) CanCapture(other ChessPiece) bool {
	cv := &CaptureVisitor{attacker: p}
	return other.AcceptCapture(cv)
}

// Rook piece
type Rook struct {
	rank, file int
	ptype      PieceType
}

func NewRook(rank int, file int) *Rook                                  { return &Rook{rank: rank, file: file, ptype: RookType} }
func (r *Rook) Rank() int                                               { return r.rank }
func (r *Rook) File() int                                               { return r.file }
func (r *Rook) Type() PieceType                                         { return r.ptype }
func (r *Rook) AcceptMove(v MoveVisitorInterface, b *ChessBoard) []Move { return v.VisitRook(r, b) }
func (r *Rook) AcceptCapture(v CaptureVisitorInterface) bool            { return v.VisitRook(r) }
func (r *Rook) GetMoves(b *ChessBoard) []Move {
	mv := &MoveVisitor{board: b}
	return r.AcceptMove(mv, b)
}
func (r *Rook) CanCapture(other ChessPiece) bool {
	cv := &CaptureVisitor{attacker: r}
	return other.AcceptCapture(cv)
}

// Knight piece
type Knight struct {
	rank, file int
	ptype      PieceType
}

func NewKnight(rank int, file int) *Knight                                { return &Knight{rank: rank, file: file, ptype: KnightType} }
func (k *Knight) Rank() int                                               { return k.rank }
func (k *Knight) File() int                                               { return k.file }
func (k *Knight) Type() PieceType                                         { return k.ptype }
func (k *Knight) AcceptMove(v MoveVisitorInterface, b *ChessBoard) []Move { return v.VisitKnight(k, b) }
func (k *Knight) AcceptCapture(v CaptureVisitorInterface) bool            { return v.VisitKnight(k) }
func (k *Knight) GetMoves(b *ChessBoard) []Move {
	mv := &MoveVisitor{board: b}
	return k.AcceptMove(mv, b)
}
func (k *Knight) CanCapture(other ChessPiece) bool {
	cv := &CaptureVisitor{attacker: k}
	return other.AcceptCapture(cv)
}

// GetMoves resolves moves for the piece by delegating to the piece's GetMoves (matches UML: ChessBoard.getMoves(IChessPiece)).
func (cb *ChessBoard) GetMoves(piece ChessPiece) []Move {
	return piece.GetMoves(cb)
}
