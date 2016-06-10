unit gResources;

interface
    uses gTypes, SwinGame, sgTypes, sysUtils;
    procedure InitResources(var game : GameData);

implementation
	// Loads all the bitmaps used in the game
	procedure LoadBitmaps();
	begin
		DebugMsg('Resources: Loading Bitmaps...');
		// Tiles
		LoadBitmapNamed('TileDirt01','tiles/dirt_1.png');
		LoadBitmapNamed('TileConcrete','tiles/concrete.png');
		LoadBitmapNamed('TileGrass01','tiles/grass_0_lp.png');
		LoadBitmapNamed('TileGrassZoned','tiles/grass_zoned.png');
		LoadBitmapNamed('TileWater01','tiles/water_0.png');
		LoadBitmapNamed('TileRoad01','tiles/road_0_lp.png');
		LoadBitmapNamed('TileRoad02','tiles/road_1_lp.png');
		LoadBitmapNamed('TileRoadIntersection','tiles/road_intersection.png');
		LoadBitmapNamed('TileTunnelLeft','tiles/tunnel_l.png');
		LoadBitmapNamed('TileTunnelRight','tiles/tunnel_r.png');
		LoadBitmapNamed('TileTunnelBottomLeft','tiles/tunnel_bl.png');
		LoadBitmapNamed('TileSlopeLeft','tiles/slope_l.png');
		LoadBitmapNamed('TileSlopeRight','tiles/slope_r.png');
		LoadBitmapNamed('TileSlopeBottomLeft','tiles/slope_bl.png');
		LoadBitmapNamed('TileSlopeBottomRight','tiles/slope_br.png');
		DebugMsg('Resources: Loaded tile bitmaps.');

		// Buildings
		LoadBitmapNamed('HQBuilding','buildings/hq.png');
		LoadBitmapNamed('GenericBuilding01','buildings/generic_0.png');
		LoadBitmapNamed('House01','buildings/house_0.png');
		LoadBitmapNamed('House01Shadow','buildings/house_1.shadow.png');
		LoadBitmapNamed('House02','buildings/house_1.png');
		LoadBitmapNamed('ShopBL01','buildings/shop_bl_1.png');
		LoadBitmapNamed('PowerPlant','buildings/powerplant.png');
		LoadBitmapNamed('WaterPump','buildings/waterpump.png');
		DebugMsg('Resources: Loaded building bitmaps.');

		LoadBitmapNamed('UISidebarTop','ui/side_top.png');
		LoadBitmapNamed('UISidebarRow','ui/side_row.png');
		LoadBitmapNamed('UISidebarBottom','ui/side_bottom.png');
		LoadBitmapNamed('UIMouseSelect','ui/mouse_select.png');
		LoadBitmapNamed('UIMouseMove','ui/mouse_move.png');
		LoadBitmapNamed('UINotification','ui/notification.png');
		LoadBitmapNamed('UIBldgHover','ui/bldg_hover.png');
		LoadBitmapNamed('UIZoneBg','ui/zone_bg.png');
		LoadBitmapNamed('UIZoneBgActive','ui/zone_bg_active.png');
		LoadBitmapNamed('UIZoneRes','ui/zone_bg_res.png');
		LoadBitmapNamed('UIZoneCom','ui/zone_bg_com.png');
		LoadBitmapNamed('UIZoneBull','ui/zone_bg_bull.png');
		LoadBitmapNamed('UIZoneRoad','ui/zone_bg_road.png');
		LoadBitmapNamed('UIZonePower','ui/zone_bg_power.png');
		LoadBitmapNamed('UIZoneWater','ui/zone_bg_water.png');
		LoadBitmapNamed('UIIconNoPower','ui/icon_nopower.png');
		LoadBitmapNamed('UIPlaceValid','ui/place_valid.png');
		LoadBitmapNamed('UIPlaceValidDouble','ui/place_valid_double.png');
		DebugMsg('Resources: Loaded UI bitmaps.');


		LoadBitmapNamed('MainMenuBackground','ui/mainmenu_bg.png');
		LoadBitmapNamed('MainMenuName','ui/mainmenu_name.png');
		LoadBitmapNamed('MainMenuPlayBtnHover','ui/mainmenu_bg_btn_play_hover.png');
		LoadBitmapNamed('MainMenuQuitBtnHover','ui/mainmenu_bg_btn_quit_hover.png');
		LoadBitmapNamed('MainMenuOkBtnHover','ui/mainmenu_bg_btn_ok_hover.png');
		DebugMsg('Resources: Loaded main menu bitmaps.');


		LoadBitmapNamed('AgentTruckRedTL', 'agents/truck_red_tl.png');
		LoadBitmapNamed('AgentTruckRedTR', 'agents/truck_red_tr.png');
		LoadBitmapNamed('AgentTruckRedBL', 'agents/truck_red_bl.png');
		LoadBitmapNamed('AgentTruckRedBR', 'agents/truck_red_br.png');
		DebugMsg('Resources: Loaded agent bitmaps.');
		DebugMsg('Resources: Loading Bitmaps... done.');

		DebugMsg('Resources: Loading game audio...');
		LoadMusicNamed('MainThemeOne', 'TSFH - I Love You Forever.mp3');
		LoadSoundEffectNamed('UIClick', 'ui_click.wav');
		LoadSoundEffectNamed('BuildRes', 'res_build.wav');
		LoadSoundEffectNamed('BuildCom', 'com_build.wav');
		LoadSoundEffectNamed('BuildPower', 'power.wav');
		LoadSoundEffectNamed('BuildWater', 'water.wav');
		LoadSoundEffectNamed('PaidNotif', 'paid.wav');
		DebugMsg('Resources: Loaded game audio.');
	end;

	// Defines a tile type
	procedure DefineTile(var game : GameData; bitmap: String; terrainType: TerrainType);
	var
		idx: Integer;
	begin
		SetLength(game.tileTypes, Length(game.tileTypes) + 1);
		idx := High(game.tileTypes);
		game.tileTypes[idx].bitmap := BitmapNamed(bitmap);
		game.tileTypes[idx].terrainType := terrainType;
	end;

	// Defines an agent type
	procedure DefineUnit(var game : GameData; img,icon : Bitmap; name : String; cost : Integer);
	var
		idx: Integer;
	begin
		SetLength(game.unitTypes, Length(game.unitTypes) + 1);
		idx := High(game.unitTypes);

		game.unitTypes[idx].img := img;
		game.unitTypes[idx].icon := icon;
		game.unitTypes[idx].name := name;
		game.unitTypes[idx].cost := cost;
	end;

	// Load all the tile types
	procedure LoadTileTypes(var game : GameData);
	begin
		DebugMsg('Resources: Loading Tile Types...');
		// DefineTile(game, bitmap name, terraintype)
		DefineTile(game, 'TileDirt01', TERRAIN_NORMAL); // 0
		DefineTile(game, 'TileGrass01', TERRAIN_NORMAL); // 1 
		DefineTile(game, 'TileWater01', TERRAIN_WATER); // 2
		DefineTile(game, 'TileRoad01', TERRAIN_ROAD); // 3
		DefineTile(game, 'TileRoad02', TERRAIN_ROAD); // 4
		DefineTile(game, 'TileGrassZoned', TERRAIN_ZONE); // 5
		// DefineTile(game, 'TileGrassZoned', TERRAIN_ZONE); // 6
		DebugMsg('Resources: Loading Tile Types... done.');
	end;

	// Load all the agent types
	procedure LoadUnitTypes(var game : GameData);
	begin
		// DefineUnit(game, bitmap, icon, name, cost)
		DefineUnit(game, BitmapNamed('AgentTruckRedTL'), BitmapNamed('AgentTruckRedTL'), 'Test Unit', 500);
	end;

	procedure LoadFonts();
	begin
		LoadFontNamed('OpenSansRegular', 'opensans_regular.ttf', 12);
		LoadFontNamed('OpenSansSemiBold', 'opensans_semibold.ttf', 12);
		LoadFontNamed('OpenSansSemiBold16', 'opensans_semibold.ttf', 16);
		LoadFontNamed('OpenSansSemiBold22', 'opensans_semibold.ttf', 22);
		LoadFontNamed('OpenSansSemiBold42', 'opensans_semibold.ttf', 42);
		LoadFontNamed('NotifHeader', 'opensans_semibold.ttf', 14);
		LoadFontNamed('NotifContent', 'opensans_regular.ttf', 12);
		LoadFontNamed('HoogUILarge', 'hoog0555.ttf', 22);
	end;

	procedure LoadNames(var game : GameData);
	var
	  tfIn: TextFile;
	  s: string;
	begin
		AssignFile(tfIn, 'names.txt');
		
		// Embed the file handling in a try/except block to handle errors gracefully
		try
		  // Open the file for reading
		  Reset(tfIn);
		
		  // Keep reading lines until the end of the file is reached
		  while not eof(tfIn) do
		  begin
		    ReadLn(tfIn, s);
		    // DebugMsg('Reading Name: '+ s);
		    SetLength(game.names, Length(game.names) + 1);
		    game.names[High(game.names)] := s;
		    SetLength(game.names[High(game.names)], Length(s) - 2);
		  end;
		
		  // Done so close the file
		  CloseFile(tfIn);
		
		except
		  on E: EInOutError do
		   WriteLn('File handling error occurred. Details: ', E.Message);
		end;
	end;

	// Calls other resource procedures
    procedure InitResources(var game : GameData);
    begin
    	DebugMsg('Resources: Initialising...');
    	LoadBitmaps();
    	LoadTileTypes(game);
    	LoadUnitTypes(game);
    	LoadNames(game);
    	LoadFonts();
    	DebugMsg('Resources: Initialising... done.');
    end;

end.
					