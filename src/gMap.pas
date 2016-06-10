 unit gMap;
//=============================================================================
// gUnitAI.pas
//=============================================================================
//
// Handles map generation, point conversions and general world computation
//
//=============================================================================

interface
	uses SysUtils, SwinGame, sgTypes, gTypes, Math; 
	procedure GenerateMap(var game : GameData);
	function WorldToScreen(point : Point2D): Point2D;
	function ScreenToWorld(point : Point2D): Point2D;
	function ScreenToReal(point : Point2D): Point2D;
	function RealToWorld(point : Point2D): Point2D;
	function WorldBuilding(point : Point2D; sprite : Sprite): Point2D;
	function WorldBuilding(point : Point2D; bitmap : Bitmap): Point2D;
	function Point(x,y: Integer): Point2D;
	function Point(x,y: Single): Point2D;
	function NewPoint(x,y: Single): Point2D;
	function PointOfInt(x,y: Integer): Point2D;
	function MapDistance(a,b : Point2D): Double;
	function PlotIsAvailable(const game : GameData; loc : Point2D): Boolean;
	function FindNeighbours(var game : GameData; pt : Point2D): TileArray;

implementation
	// Loads a tile into the game world
	procedure LoadTile(var game : GameData; point : Point2D; t : Integer);
	begin
		game.map[Round(point.x), Round(point.y)].tType := game.tileTypes[t];
		game.map[Round(point.x), Round(point.y)].bitmap := game.map[Round(point.x), Round(point.y)].tType.bitmap;
		game.map[Round(point.x), Round(point.y)].loc := point;
		game.map[Round(point.x), Round(point.y)].hasBuilding := false;
	end;

	// Generates the map tiles
	procedure GenerateMap(var game : GameData);
	var
		x,y: Integer;
	begin
		DebugMsg('Map: Generating tiles...');
		DebugMsg('Map: Width - ' + IntToStr(MAP_WIDTH) + ' tiles');
		DebugMsg('Map: Height - ' + IntToStr(MAP_HEIGHT) + ' tiles');
		for x := 0 to MAP_WIDTH do
		begin
			for y := 0 to MAP_HEIGHT do
			begin
				// @todo Dynamic map generation
				// WriteLn(IntToStr(Length(game.tileTypes)));
				game.map[x,y].tType := game.tileTypes[1];
				game.map[x,y].bitmap := game.map[x,y].tType.bitmap;
				game.map[x,y].hasBuilding := false;
			end;
		end;

		LoadTile(game, Point(0,5), 3);
		LoadTile(game, Point(0,10), 3);
		LoadTile(game, Point(0,15), 3);
		LoadTile(game, Point(0,20), 3);
		LoadTile(game, Point(0,25), 3);
		LoadTile(game, Point(0,30), 3);
		LoadTile(game, Point(1,5), 3);
		LoadTile(game, Point(1,10), 3);
		LoadTile(game, Point(1,15), 3);
		LoadTile(game, Point(1,20), 3);
		LoadTile(game, Point(1,25), 3);
		LoadTile(game, Point(1,30), 3);
		LoadTile(game, Point(2,5), 3);
		LoadTile(game, Point(2,10), 3);
		LoadTile(game, Point(2,15), 3);
		LoadTile(game, Point(2,20), 3);
		LoadTile(game, Point(2,25), 3);
		LoadTile(game, Point(2,30), 3);
		LoadTile(game, Point(3,5), 3);
		LoadTile(game, Point(3,10), 3);
		LoadTile(game, Point(3,15), 3);
		LoadTile(game, Point(3,20), 3);
		LoadTile(game, Point(3,25), 3);
		LoadTile(game, Point(3,30), 3);
		LoadTile(game, Point(4,5), 3);
		LoadTile(game, Point(4,10), 3);
		LoadTile(game, Point(4,15), 3);
		LoadTile(game, Point(4,20), 3);
		LoadTile(game, Point(4,25), 3);
		LoadTile(game, Point(4,30), 3);
		LoadTile(game, Point(5,0), 4);
		LoadTile(game, Point(5,1), 4);
		LoadTile(game, Point(5,2), 4);
		LoadTile(game, Point(5,3), 4);
		LoadTile(game, Point(5,4), 4);
		LoadTile(game, Point(5,5), 3);
		LoadTile(game, Point(5,5), 4);
		LoadTile(game, Point(5,6), 4);
		LoadTile(game, Point(5,7), 4);
		LoadTile(game, Point(5,8), 4);
		LoadTile(game, Point(5,9), 4);
		LoadTile(game, Point(5,10), 3);
		LoadTile(game, Point(5,10), 4);
		LoadTile(game, Point(5,11), 4);
		LoadTile(game, Point(5,12), 4);
		LoadTile(game, Point(5,13), 4);
		LoadTile(game, Point(5,14), 4);
		LoadTile(game, Point(5,15), 3);
		LoadTile(game, Point(5,15), 4);
		LoadTile(game, Point(5,16), 4);
		LoadTile(game, Point(5,17), 4);
		LoadTile(game, Point(5,18), 4);
		LoadTile(game, Point(5,19), 4);
		LoadTile(game, Point(5,20), 3);
		LoadTile(game, Point(5,20), 4);
		LoadTile(game, Point(5,21), 4);
		LoadTile(game, Point(5,22), 4);
		LoadTile(game, Point(5,23), 4);
		LoadTile(game, Point(5,24), 4);
		LoadTile(game, Point(5,25), 3);
		LoadTile(game, Point(5,25), 4);
		LoadTile(game, Point(5,26), 4);
		LoadTile(game, Point(5,27), 4);
		LoadTile(game, Point(5,28), 4);
		LoadTile(game, Point(5,29), 4);
		LoadTile(game, Point(5,30), 3);
		LoadTile(game, Point(5,30), 4);
		LoadTile(game, Point(5,31), 4);
		LoadTile(game, Point(5,32), 4);
		LoadTile(game, Point(6,5), 3);
		LoadTile(game, Point(6,10), 3);
		LoadTile(game, Point(6,15), 3);
		LoadTile(game, Point(6,20), 3);
		LoadTile(game, Point(6,25), 3);
		LoadTile(game, Point(6,30), 3);
		LoadTile(game, Point(7,5), 3);
		LoadTile(game, Point(7,10), 3);
		LoadTile(game, Point(7,15), 3);
		LoadTile(game, Point(7,20), 3);
		LoadTile(game, Point(7,25), 3);
		LoadTile(game, Point(7,30), 3);
		LoadTile(game, Point(8,5), 3);
		LoadTile(game, Point(8,10), 3);
		LoadTile(game, Point(8,15), 3);
		LoadTile(game, Point(8,20), 3);
		LoadTile(game, Point(8,25), 3);
		LoadTile(game, Point(8,30), 3);
		LoadTile(game, Point(9,5), 3);
		LoadTile(game, Point(9,10), 3);
		LoadTile(game, Point(9,15), 3);
		LoadTile(game, Point(9,20), 3);
		LoadTile(game, Point(9,25), 3);
		LoadTile(game, Point(9,30), 3);
		LoadTile(game, Point(10,0), 4);
		LoadTile(game, Point(10,1), 4);
		LoadTile(game, Point(10,2), 4);
		LoadTile(game, Point(10,3), 4);
		LoadTile(game, Point(10,4), 4);
		LoadTile(game, Point(10,5), 3);
		LoadTile(game, Point(10,5), 4);
		LoadTile(game, Point(10,6), 4);
		LoadTile(game, Point(10,7), 4);
		LoadTile(game, Point(10,8), 4);
		LoadTile(game, Point(10,9), 4);
		LoadTile(game, Point(10,10), 3);
		LoadTile(game, Point(10,10), 4);
		LoadTile(game, Point(10,11), 4);
		LoadTile(game, Point(10,12), 4);
		LoadTile(game, Point(10,13), 4);
		LoadTile(game, Point(10,14), 4);
		LoadTile(game, Point(10,15), 3);
		LoadTile(game, Point(10,15), 4);
		LoadTile(game, Point(10,16), 4);
		LoadTile(game, Point(10,17), 4);
		LoadTile(game, Point(10,18), 4);
		LoadTile(game, Point(10,19), 4);
		LoadTile(game, Point(10,20), 3);
		LoadTile(game, Point(10,20), 4);
		LoadTile(game, Point(10,21), 4);
		LoadTile(game, Point(10,22), 4);
		LoadTile(game, Point(10,23), 4);
		LoadTile(game, Point(10,24), 4);
		LoadTile(game, Point(10,25), 3);
		LoadTile(game, Point(10,25), 4);
		LoadTile(game, Point(10,26), 4);
		LoadTile(game, Point(10,27), 4);
		LoadTile(game, Point(10,28), 4);
		LoadTile(game, Point(10,29), 4);
		LoadTile(game, Point(10,30), 3);
		LoadTile(game, Point(10,30), 4);
		LoadTile(game, Point(10,31), 4);
		LoadTile(game, Point(10,32), 4);
		LoadTile(game, Point(11,5), 3);
		LoadTile(game, Point(11,10), 3);
		LoadTile(game, Point(11,11), 2);
		LoadTile(game, Point(11,12), 2);
		LoadTile(game, Point(11,13), 2);
		LoadTile(game, Point(11,14), 2);
		LoadTile(game, Point(11,15), 3);
		LoadTile(game, Point(11,15), 2);
		LoadTile(game, Point(11,16), 2);
		LoadTile(game, Point(11,17), 2);
		LoadTile(game, Point(11,18), 2);
		LoadTile(game, Point(11,19), 2);
		LoadTile(game, Point(11,20), 3);
		LoadTile(game, Point(11,25), 3);
		LoadTile(game, Point(11,30), 3);
		LoadTile(game, Point(12,5), 3);
		LoadTile(game, Point(12,10), 3);
		LoadTile(game, Point(12,11), 2);
		LoadTile(game, Point(12,12), 2);
		LoadTile(game, Point(12,13), 2);
		LoadTile(game, Point(12,14), 2);
		LoadTile(game, Point(12,15), 3);
		LoadTile(game, Point(12,15), 2);
		LoadTile(game, Point(12,16), 2);
		LoadTile(game, Point(12,17), 2);
		LoadTile(game, Point(12,18), 2);
		LoadTile(game, Point(12,19), 2);
		LoadTile(game, Point(12,20), 3);
		LoadTile(game, Point(12,25), 3);
		LoadTile(game, Point(12,30), 3);
		LoadTile(game, Point(13,5), 3);
		LoadTile(game, Point(13,10), 3);
		LoadTile(game, Point(13,11), 2);
		LoadTile(game, Point(13,12), 2);
		LoadTile(game, Point(13,13), 2);
		LoadTile(game, Point(13,14), 2);
		LoadTile(game, Point(13,15), 3);
		LoadTile(game, Point(13,15), 2);
		LoadTile(game, Point(13,16), 2);
		LoadTile(game, Point(13,17), 2);
		LoadTile(game, Point(13,18), 2);
		LoadTile(game, Point(13,19), 2);
		LoadTile(game, Point(13,20), 3);
		LoadTile(game, Point(13,25), 3);
		LoadTile(game, Point(13,30), 3);
		LoadTile(game, Point(14,5), 3);
		LoadTile(game, Point(14,10), 3);
		LoadTile(game, Point(14,11), 2);
		LoadTile(game, Point(14,12), 2);
		LoadTile(game, Point(14,13), 2);
		LoadTile(game, Point(14,14), 2);
		LoadTile(game, Point(14,15), 3);
		LoadTile(game, Point(14,15), 2);
		LoadTile(game, Point(14,16), 2);
		LoadTile(game, Point(14,17), 2);
		LoadTile(game, Point(14,18), 2);
		LoadTile(game, Point(14,19), 2);
		LoadTile(game, Point(14,20), 3);
		LoadTile(game, Point(14,25), 3);
		LoadTile(game, Point(14,30), 3);
		LoadTile(game, Point(15,0), 4);
		LoadTile(game, Point(15,1), 4);
		LoadTile(game, Point(15,2), 4);
		LoadTile(game, Point(15,3), 4);
		LoadTile(game, Point(15,4), 4);
		LoadTile(game, Point(15,5), 3);
		LoadTile(game, Point(15,5), 4);
		LoadTile(game, Point(15,6), 4);
		LoadTile(game, Point(15,7), 4);
		LoadTile(game, Point(15,8), 4);
		LoadTile(game, Point(15,9), 4);
		LoadTile(game, Point(15,10), 3);
		LoadTile(game, Point(15,10), 4);
		LoadTile(game, Point(15,11), 4);
		LoadTile(game, Point(15,11), 2);
		LoadTile(game, Point(15,12), 4);
		LoadTile(game, Point(15,12), 2);
		LoadTile(game, Point(15,13), 4);
		LoadTile(game, Point(15,13), 2);
		LoadTile(game, Point(15,14), 4);
		LoadTile(game, Point(15,14), 2);
		LoadTile(game, Point(15,15), 3);
		LoadTile(game, Point(15,15), 4);
		LoadTile(game, Point(15,15), 2);
		LoadTile(game, Point(15,16), 4);
		LoadTile(game, Point(15,16), 2);
		LoadTile(game, Point(15,17), 4);
		LoadTile(game, Point(15,17), 2);
		LoadTile(game, Point(15,18), 4);
		LoadTile(game, Point(15,18), 2);
		LoadTile(game, Point(15,19), 4);
		LoadTile(game, Point(15,19), 2);
		LoadTile(game, Point(15,20), 3);
		LoadTile(game, Point(15,20), 4);
		LoadTile(game, Point(15,21), 4);
		LoadTile(game, Point(15,22), 4);
		LoadTile(game, Point(15,23), 4);
		LoadTile(game, Point(15,24), 4);
		LoadTile(game, Point(15,25), 3);
		LoadTile(game, Point(15,25), 4);
		LoadTile(game, Point(15,26), 4);
		LoadTile(game, Point(15,27), 4);
		LoadTile(game, Point(15,28), 4);
		LoadTile(game, Point(15,29), 4);
		LoadTile(game, Point(15,30), 3);
		LoadTile(game, Point(15,30), 4);
		LoadTile(game, Point(15,31), 4);
		LoadTile(game, Point(15,32), 4);
		LoadTile(game, Point(16,5), 3);
		LoadTile(game, Point(16,10), 3);
		LoadTile(game, Point(16,11), 2);
		LoadTile(game, Point(16,12), 2);
		LoadTile(game, Point(16,13), 2);
		LoadTile(game, Point(16,14), 2);
		LoadTile(game, Point(16,15), 3);
		LoadTile(game, Point(16,15), 2);
		LoadTile(game, Point(16,16), 2);
		LoadTile(game, Point(16,17), 2);
		LoadTile(game, Point(16,18), 2);
		LoadTile(game, Point(16,19), 2);
		LoadTile(game, Point(16,20), 3);
		LoadTile(game, Point(16,25), 3);
		LoadTile(game, Point(16,30), 3);
		LoadTile(game, Point(17,5), 3);
		LoadTile(game, Point(17,10), 3);
		LoadTile(game, Point(17,11), 2);
		LoadTile(game, Point(17,12), 2);
		LoadTile(game, Point(17,13), 2);
		LoadTile(game, Point(17,14), 2);
		LoadTile(game, Point(17,15), 3);
		LoadTile(game, Point(17,15), 2);
		LoadTile(game, Point(17,16), 2);
		LoadTile(game, Point(17,17), 2);
		LoadTile(game, Point(17,18), 2);
		LoadTile(game, Point(17,19), 2);
		LoadTile(game, Point(17,20), 3);
		LoadTile(game, Point(17,25), 3);
		LoadTile(game, Point(17,30), 3);
		LoadTile(game, Point(18,5), 3);
		LoadTile(game, Point(18,10), 3);
		LoadTile(game, Point(18,11), 2);
		LoadTile(game, Point(18,12), 2);
		LoadTile(game, Point(18,13), 2);
		LoadTile(game, Point(18,14), 2);
		LoadTile(game, Point(18,15), 3);
		LoadTile(game, Point(18,15), 2);
		LoadTile(game, Point(18,16), 2);
		LoadTile(game, Point(18,17), 2);
		LoadTile(game, Point(18,18), 2);
		LoadTile(game, Point(18,19), 2);
		LoadTile(game, Point(18,20), 3);
		LoadTile(game, Point(18,25), 3);
		LoadTile(game, Point(18,30), 3);
		LoadTile(game, Point(19,5), 3);
		LoadTile(game, Point(19,10), 3);
		LoadTile(game, Point(19,11), 2);
		LoadTile(game, Point(19,12), 2);
		LoadTile(game, Point(19,13), 2);
		LoadTile(game, Point(19,14), 2);
		LoadTile(game, Point(19,15), 3);
		LoadTile(game, Point(19,15), 2);
		LoadTile(game, Point(19,16), 2);
		LoadTile(game, Point(19,17), 2);
		LoadTile(game, Point(19,18), 2);
		LoadTile(game, Point(19,19), 2);
		LoadTile(game, Point(19,20), 3);
		LoadTile(game, Point(19,25), 3);
		LoadTile(game, Point(19,30), 3);
		LoadTile(game, Point(20,0), 4);
		LoadTile(game, Point(20,1), 4);
		LoadTile(game, Point(20,2), 4);
		LoadTile(game, Point(20,3), 4);
		LoadTile(game, Point(20,4), 4);
		LoadTile(game, Point(20,5), 3);
		LoadTile(game, Point(20,5), 4);
		LoadTile(game, Point(20,6), 4);
		LoadTile(game, Point(20,7), 4);
		LoadTile(game, Point(20,8), 4);
		LoadTile(game, Point(20,9), 4);
		LoadTile(game, Point(20,10), 3);
		LoadTile(game, Point(20,10), 4);
		LoadTile(game, Point(20,11), 4);
		LoadTile(game, Point(20,12), 4);
		LoadTile(game, Point(20,13), 4);
		LoadTile(game, Point(20,14), 4);
		LoadTile(game, Point(20,15), 3);
		LoadTile(game, Point(20,15), 4);
		LoadTile(game, Point(20,16), 4);
		LoadTile(game, Point(20,17), 4);
		LoadTile(game, Point(20,18), 4);
		LoadTile(game, Point(20,19), 4);
		LoadTile(game, Point(20,20), 3);
		LoadTile(game, Point(20,20), 4);
		LoadTile(game, Point(20,21), 4);
		LoadTile(game, Point(20,22), 4);
		LoadTile(game, Point(20,23), 4);
		LoadTile(game, Point(20,24), 4);
		LoadTile(game, Point(20,25), 3);
		LoadTile(game, Point(20,25), 4);
		LoadTile(game, Point(20,26), 4);
		LoadTile(game, Point(20,27), 4);
		LoadTile(game, Point(20,28), 4);
		LoadTile(game, Point(20,29), 4);
		LoadTile(game, Point(20,30), 3);
		LoadTile(game, Point(20,30), 4);
		LoadTile(game, Point(20,31), 4);
		LoadTile(game, Point(20,32), 4);
		LoadTile(game, Point(21,5), 3);
		LoadTile(game, Point(21,10), 3);
		LoadTile(game, Point(21,15), 3);
		LoadTile(game, Point(21,20), 3);
		LoadTile(game, Point(21,25), 3);
		LoadTile(game, Point(21,30), 3);
		LoadTile(game, Point(22,5), 3);
		LoadTile(game, Point(22,10), 3);
		LoadTile(game, Point(22,15), 3);
		LoadTile(game, Point(22,20), 3);
		LoadTile(game, Point(22,25), 3);
		LoadTile(game, Point(22,30), 3);
		LoadTile(game, Point(23,5), 3);
		LoadTile(game, Point(23,10), 3);
		LoadTile(game, Point(23,15), 3);
		LoadTile(game, Point(23,20), 3);
		LoadTile(game, Point(23,25), 3);
		LoadTile(game, Point(23,30), 3);
		LoadTile(game, Point(24,5), 3);
		LoadTile(game, Point(24,10), 3);
		LoadTile(game, Point(24,15), 3);
		LoadTile(game, Point(24,20), 3);
		LoadTile(game, Point(24,25), 3);
		LoadTile(game, Point(24,30), 3);
		LoadTile(game, Point(25,0), 4);
		LoadTile(game, Point(25,1), 4);
		LoadTile(game, Point(25,2), 4);
		LoadTile(game, Point(25,3), 4);
		LoadTile(game, Point(25,4), 4);
		LoadTile(game, Point(25,5), 3);
		LoadTile(game, Point(25,5), 4);
		LoadTile(game, Point(25,6), 4);
		LoadTile(game, Point(25,7), 4);
		LoadTile(game, Point(25,8), 4);
		LoadTile(game, Point(25,9), 4);
		LoadTile(game, Point(25,10), 3);
		LoadTile(game, Point(25,10), 4);
		LoadTile(game, Point(25,11), 4);
		LoadTile(game, Point(25,12), 4);
		LoadTile(game, Point(25,13), 4);
		LoadTile(game, Point(25,14), 4);
		LoadTile(game, Point(25,15), 3);
		LoadTile(game, Point(25,15), 4);
		LoadTile(game, Point(25,16), 4);
		LoadTile(game, Point(25,17), 4);
		LoadTile(game, Point(25,18), 4);
		LoadTile(game, Point(25,19), 4);
		LoadTile(game, Point(25,20), 3);
		LoadTile(game, Point(25,20), 4);
		LoadTile(game, Point(25,21), 4);
		LoadTile(game, Point(25,22), 4);
		LoadTile(game, Point(25,23), 4);
		LoadTile(game, Point(25,24), 4);
		LoadTile(game, Point(25,25), 3);
		LoadTile(game, Point(25,25), 4);
		LoadTile(game, Point(25,26), 4);
		LoadTile(game, Point(25,27), 4);
		LoadTile(game, Point(25,28), 4);
		LoadTile(game, Point(25,29), 4);
		LoadTile(game, Point(25,30), 3);
		LoadTile(game, Point(25,30), 4);
		LoadTile(game, Point(25,31), 4);
		LoadTile(game, Point(25,32), 4);
		LoadTile(game, Point(26,5), 3);
		LoadTile(game, Point(26,10), 3);
		LoadTile(game, Point(26,15), 3);
		LoadTile(game, Point(26,20), 3);
		LoadTile(game, Point(26,25), 3);
		LoadTile(game, Point(26,30), 3);
		LoadTile(game, Point(27,5), 3);
		LoadTile(game, Point(27,10), 3);
		LoadTile(game, Point(27,15), 3);
		LoadTile(game, Point(27,20), 3);
		LoadTile(game, Point(27,25), 3);
		LoadTile(game, Point(27,30), 3);
		LoadTile(game, Point(28,5), 3);
		LoadTile(game, Point(28,10), 3);
		LoadTile(game, Point(28,15), 3);
		LoadTile(game, Point(28,20), 3);
		LoadTile(game, Point(28,25), 3);
		LoadTile(game, Point(28,30), 3);
		LoadTile(game, Point(29,5), 3);
		LoadTile(game, Point(29,10), 3);
		LoadTile(game, Point(29,15), 3);
		LoadTile(game, Point(29,20), 3);
		LoadTile(game, Point(29,25), 3);
		LoadTile(game, Point(29,30), 3);
		LoadTile(game, Point(30,0), 4);
		LoadTile(game, Point(30,1), 4);
		LoadTile(game, Point(30,2), 4);
		LoadTile(game, Point(30,3), 4);
		LoadTile(game, Point(30,4), 4);
		LoadTile(game, Point(30,5), 3);
		LoadTile(game, Point(30,5), 4);
		LoadTile(game, Point(30,6), 4);
		LoadTile(game, Point(30,7), 4);
		LoadTile(game, Point(30,8), 4);
		LoadTile(game, Point(30,9), 4);
		LoadTile(game, Point(30,10), 3);
		LoadTile(game, Point(30,10), 4);
		LoadTile(game, Point(30,11), 4);
		LoadTile(game, Point(30,12), 4);
		LoadTile(game, Point(30,13), 4);
		LoadTile(game, Point(30,14), 4);
		LoadTile(game, Point(30,15), 3);
		LoadTile(game, Point(30,15), 4);
		LoadTile(game, Point(30,16), 4);
		LoadTile(game, Point(30,17), 4);
		LoadTile(game, Point(30,18), 4);
		LoadTile(game, Point(30,19), 4);
		LoadTile(game, Point(30,20), 3);
		LoadTile(game, Point(30,20), 4);
		LoadTile(game, Point(30,21), 4);
		LoadTile(game, Point(30,22), 4);
		LoadTile(game, Point(30,23), 4);
		LoadTile(game, Point(30,24), 4);
		LoadTile(game, Point(30,25), 3);
		LoadTile(game, Point(30,25), 4);
		LoadTile(game, Point(30,26), 4);
		LoadTile(game, Point(30,27), 4);
		LoadTile(game, Point(30,28), 4);
		LoadTile(game, Point(30,29), 4);
		LoadTile(game, Point(30,30), 3);
		LoadTile(game, Point(30,30), 4);
		LoadTile(game, Point(30,31), 4);
		LoadTile(game, Point(30,32), 4);
		LoadTile(game, Point(31,5), 3);
		LoadTile(game, Point(31,10), 3);
		LoadTile(game, Point(31,15), 3);
		LoadTile(game, Point(31,20), 3);
		LoadTile(game, Point(31,25), 3);
		LoadTile(game, Point(31,30), 3);
		LoadTile(game, Point(32,5), 3);
		LoadTile(game, Point(32,10), 3);
		LoadTile(game, Point(32,15), 3);
		LoadTile(game, Point(32,20), 3);
		LoadTile(game, Point(32,25), 3);
		LoadTile(game, Point(32,30), 3);

		for x := 0 to MAP_WIDTH do
		begin
			for y := 0 to MAP_HEIGHT do
			begin
				// @todo Dynamic map generation
				// WriteLn(IntToStr(Length(game.tileTypes)));
				// game.map[x,y].tType := game.tileTypes[1];
				// game.map[x,y].bitmap := game.map[x,y].tType.bitmap;

				// if (x = 6) and (y = 5) then
				// begin
				// 	game.map[x,y].bitmap := BitmapNamed('HQBuilding');
				// end;

				// if (y mod 5 = 0) and (y > 0) then
				// begin
				// 	game.map[x,y].tType := game.tileTypes[3];
				// 	game.map[x,y].bitmap := game.map[x,y].tType.bitmap;
				// 	WriteLn('LoadTile(game, Point('+IntToStr(x)+','+IntToStr(y)+'), '+IntToStr(3)+');');
				// end;

				// if (x mod 5 = 0) and (x > 0) then
				// begin
				// 	game.map[x,y].tType := game.tileTypes[4];
				// 	game.map[x,y].bitmap := game.map[x,y].tType.bitmap;
				// 	WriteLn('LoadTile(game, Point('+IntToStr(x)+','+IntToStr(y)+'), '+IntToStr(4)+');');
				// end;

				// if (x > 10) and (y > 10) and (x < 20) and (y < 20) then
				// begin
				// 	game.map[x,y].tType := game.tileTypes[2];
				// 	game.map[x,y].bitmap := game.map[x,y].tType.bitmap;
				// 	WriteLn('LoadTile(game, Point('+IntToStr(x)+','+IntToStr(y)+'), '+IntToStr(2)+');');
				// 	continue;
				// end;

				if (x mod 5 = 0) and (x >0) and (y mod 5 = 0) and (y > 0) and not ((x = 15) and (y= 15)) then
				begin
					// game.map[x,y].tType := game.tileTypes[4];
					game.map[x,y].bitmap := BitmapNamed('TileRoadIntersection');
				end;
				
				if (Random(1000) > 950) then
				begin
					// game.map[x,y].tType := game.tileTypes[2];
					// game.map[x,y].bitmap := BitmapNamed('GenericBuilding01');
				end;

			end;
		end;

		DebugMsg('Map: Generating tiles... done.');
	end;

	// Convert a map x,y to a screen x,y
	function WorldToScreen(point : Point2D): Point2D;
	begin
		// point.y := point.y + 1;
		result.x := Round((point.x * TILE_WIDTH  / 2) + (point.y * TILE_WIDTH  / 2));
		result.y := Round((point.y * TILE_HEIGHT / 2) - (point.x * TILE_HEIGHT / 2));
	end;

	// Convert a screen x,y to a map x,y
	function ScreenToWorld(point : Point2D): Point2D;
	begin
		// Since we can move the camera, we need to remap the 2d screen 
		// point to the absolute position of the mouse
		point.x := point.x - ((point.x - CameraX()) - point.x);
		point.y := point.y - ((point.y - CameraY()) - point.y);

		// Calculate the world x,y coords, will return the tile the
		// point is in 
		result.x := Round((point.x / TILE_WIDTH) - ((point.y - (TILE_HEIGHT / 2)) / 32) - 0.5);
		result.y := Round((point.x / TILE_WIDTH) + ((point.y - (TILE_HEIGHT / 2)) / 32) - 0.5);
	end;

	function ScreenToReal(point : Point2D): Point2D;
	begin
		result.x := point.x - ((point.x - CameraX()) - point.x);
		result.y := point.y - ((point.y - CameraY()) - point.y);
	end;

	function RealToScreen(point : Point2D) : Integer;
	begin
		
	end;

	function RealToWorld(point : Point2D): Point2D;
	begin
		result.x := Round((point.x / TILE_WIDTH) - ((point.y - (TILE_HEIGHT / 2)) / 32) - 0.5);
		result.y := Round((point.x / TILE_WIDTH) + ((point.y - (TILE_HEIGHT / 2)) / 32) - 0.5);
	end;

	// Get the origin point of a building, accounting for the building height
	function WorldBuilding(point : Point2D; sprite : Sprite): Point2D;
	var
		tempX, tempY: Single;
		temp : Point2D;
	begin
		point.y := point.y + 2;
		temp := WorldToScreen(point);
		result.x := temp.x - SpriteWidth(sprite);
		result.y := temp.y - SpriteHeight(sprite);

		if SpriteWidth(sprite) = 128 then
		begin
			result.x := result.x + 32;
		end;
	end;

	// Get the origin point of a building, accounting for the building height
	function WorldBuilding(point : Point2D; bitmap : Bitmap): Point2D;
	var
		tempX, tempY: Single;
		temp : Point2D;
	begin
		point.y := point.y + 2;
		temp := WorldToScreen(point);
		result.x := temp.x - BitmapWidth(bitmap);
		result.y := temp.y - BitmapHeight(bitmap);
	end;

	// Make a Point2D value using 2 integers
	function Point(x,y: Integer): Point2D;
	begin
		result.x := x;
		result.y := y;
	end;

	// Make a Point2D value using 2 singles
	function Point(x,y: Single): Point2D;
	begin
		result.x := x;
		result.y := y;
	end;

	// Make a Point2D value using 2 singles
	function NewPoint(x,y: Single): Point2D;
	begin
		result.x := x;
		result.y := y;
	end;

	// Make a Point2D value using 2 integers
	function PointOfInt(x,y: Integer): Point2D;
	begin
		result.x := x;
		result.y := y;
	end;

	// Manhattan distance between two points on map
	function MapDistance(a,b : Point2D): Double;
	begin
		result := Abs(b.x - a.x) + Abs(b.y - a.y);
	end;

	// Get neighbouring tile for a certain tile
	function FindNeighbours(var game : GameData; pt : Point2D): TileArray;
	var
		neighbours: TileArray;
		currMapTile : Point2D;
	begin
			currMapTile := pt;

			if (currMapTile.x - 1 > 0) and (currMapTile.y < MAP_HEIGHT) then
			begin
				SetLength(neighbours, Length(neighbours) + 1);
				neighbours[High(neighbours)] := game.map[Round(currMapTile.x) - 1, Round(currMapTile.y)];
				neighbours[High(neighbours)].loc := PointOfInt(Round(currMapTile.x) - 1, Round(currMapTile.y));
			end;

			if (currMapTile.x >= 0) and (currMapTile.y + 1 < MAP_HEIGHT) then
			begin
				SetLength(neighbours, Length(neighbours) + 1);
				neighbours[High(neighbours)] := game.map[Round(currMapTile.x), Round(currMapTile.y) + 1];
				neighbours[High(neighbours)].loc := PointOfInt(Round(currMapTile.x), Round(currMapTile.y) + 1);
			end;

			if (currMapTile.x >= 0) and (currMapTile.y - 1 > 0) then
			begin
				SetLength(neighbours, Length(neighbours) + 1);
				neighbours[High(neighbours)] := game.map[Round(currMapTile.x), Round(currMapTile.y) - 1];
				neighbours[High(neighbours)].loc := PointOfInt(Round(currMapTile.x), Round(currMapTile.y) - 1);
			end;

			if (currMapTile.x + 1 < MAP_WIDTH) and (currMapTile.y < MAP_HEIGHT) then
			begin
				SetLength(neighbours, Length(neighbours) + 1);
				neighbours[High(neighbours)] := game.map[Round(currMapTile.x) + 1, Round(currMapTile.y)];
				neighbours[High(neighbours)].loc := PointOfInt(Round(currMapTile.x) + 1, Round(currMapTile.y));
			end;

			result := neighbours;
	end;

	// Check if the tile is valid for zoning
	function PlotIsPlaceable(const game : GameData; loc : Point2D): Boolean;
	begin
		if (loc.x >= 0) and (loc.y >= 0 ) and (game.map[Round(loc.x),Round(loc.y)].tType.terrainType = TERRAIN_NORMAL) then
			exit (true);

		exit (false);
	end;

	// Check if the tile doesn't have any buildings on it
	function PlotIsAvailable(const game : GameData; loc : Point2D): Boolean;
	var
		i: Integer;
		avail : Boolean;
	begin
		avail := true;
		for i := 0 to High(game.buildings) do
		begin
			if VectorsEqual(game.buildings[i].loc, loc) then
			begin
				avail := false;
			end;
		end;
		if avail then
		begin
			if not PlotIsPlaceable(game, loc) then
				exit (false);
		end;
		exit (avail);
	end;
end.
