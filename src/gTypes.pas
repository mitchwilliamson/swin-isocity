unit gTypes;
//=============================================================================
// gTypes.pas
//=============================================================================
//
// Responsible for declaring the custom types used within the game, as well as
// declaring constant values used throughout the application.
//
//=============================================================================

interface
	uses sgTypes;

	const
		SCREEN_WIDTH = 1280; // Screen Width in pixels (default 1280)
		SCREEN_HEIGHT = 720; // Screen Height in pixels (default 720)
		MAP_WIDTH = 32; // # of tiles wide the map is
		MAP_HEIGHT = 32; // # of tiles high the map is
		TILE_WIDTH = 64; // The width of the tile bitmaps in pixels (default 64)
		TILE_HEIGHT = 32; // The height of the tile bitmps in pixels (default 32)
		DEBUG_MODE = false; // Whether debug output is enabled or not
		DRAW_TILE_NUMBERS = false; // Whether tiles draw their x,y coords
		CAMERA_SPEED = 5; // How fast the camera moves
		MOUSE_MOVE_PADDING = 10; // The padding for camera movement
		START_FULLSCREEN = false; // Whether the program starts in fullscreen
		APP_NAME = 'Isocity'; // The title of the program
		SHOW_SPLASHSCREEN = false; // Whether to show SwinGame splashscreen
		PAY_TIME = 120; // Number of seconds between income pay

	type
		TerrainType = (TERRAIN_NORMAL, TERRAIN_NOACCESS, TERRAIN_WATER, TERRAIN_ZONE, TERRAIN_COMZONE, TERRAIN_ROAD, TERRAIN_BUILDING, TERRAIN_REMOVE, TERRAIN_POWER, TERRAIN_PLACEWATER);
		BuildingType = (NOT_APPLICABLE, RES_SMALL, RES_MEDIUM, RES_LARGE, COM_SMALL, COM_MEDIUM, COM_LARGE, POWER, WATER);
		MouseStatus = (MOUSE_NORMAL, MOUSE_SELECTED, MOUSE_MOVE);
		GUIStatus = (MAIN_MENU, IN_GAME, SHUTDOWN);
		MenuStatus = (MAIN_SCREEN, CITY_NAME);

		BuildingEntity = record
			sprite : Sprite;
			buildingType : BuildingType;
			// team: TeamType;
			selected: Boolean;
			mouseOver: Boolean;
			hp: Integer;
			loc: Point2D;
			timer : Timer;
			// production_queue: Array of QueueItem;
		end;

		TileType = record
			bitmap : Bitmap;
			terrainType: TerrainType;
		end;
		TileTypes = Array of TileType;
		Tile = record
			bitmap : Bitmap;
			tType : TileType;
			terrainType: TerrainType;
			loc : Point2D; // unused for map
			hasBuilding : Boolean;
			building: BuildingEntity;
		end;
		TileArray = Array of Tile;
		NodePointer	= ^Node;
		Node = record 
			loc : Point2D;
			gScore, fCost, hScore : Double;
			parent : Integer;
		end;
		NodeArray = Array of Node;

		MapData = Array[0..MAP_WIDTH,0..MAP_HEIGHT] of Tile;

		UnitType = record
			img : Bitmap;
			icon : Bitmap;
			name : String;
			cost : Integer;
			requires : BuildingType;
			damage : Integer;
		end;

		Citizen = record
			home, work : Point2D;
			firstName, lastName : String;
		end;
		Citizens = Array of Citizen;

		UnitEntity = record
			bitmap : Bitmap;
			sprite : Sprite;
			selected : Boolean;
			hp: Integer;
			loc: Point2D;
			movingTo : Point2D;
			destination : Point2D;
			moveSpeed : Single;
			unitType : UnitType;
			toDelete : Boolean;
			mouseOver: Boolean;
			path: NodeArray;
			hasPath: Boolean;
		end;

		Building = record
			img : Bitmap;
			icon : String;
			name : String;
			buildingType : BuildingType;
			// team : TeamType;
			cost : Integer;
			requires : BuildingType;
		end;

		GameMouseLocation = record
			startX, startY, endX, endY : Single;
			hasSelected : Boolean;
			placingBuilding : Boolean;
			mouseStatus : MouseStatus;
		end;

		DeleteQueue = Array of Integer;

		GameData = record
			map : MapData;
			tileTypes: TileTypes;
			mouseLoc: GameMouseLocation;
			unitTypes: Array of UnitType;
			units : Array of UnitEntity;
			buildingTypes: Array of Building;
			buildings: Array of BuildingEntity;
			power, money : Integer;
			guiStatus : GUIStatus;
			menuStatus : MenuStatus;
			citizens : Citizens;
			names : Array of String;
			cityName : String;
			zoningType: TerrainType;
			moneyTimer : Timer;
			expenses : Array of Integer;
		end;

	const
		STARTUP_MODE = MAIN_MENU; // Which mode the game starts in

	procedure DebugMsg(msg : String);

implementation
	procedure DebugMsg(msg : String);
	begin
		if DEBUG_MODE then
			WriteLn(msg);
	end;
end.
