unit gBuilding;
//=============================================================================
// gBuilding.pas
//=============================================================================
//
// Responsible for building-related functionality
//
//=============================================================================

interface
	uses SwinGame, sgTypes, gTypes, gCitizens, gMap;

	procedure SpawnBuilding(var game : GameData; loc : Point2D; terrainType : TerrainType);
	function GetCitizensForBuilding(var game : GameData; loc : Point2D) : Citizens;
implementation

	// Spawns a building 
	procedure SpawnBuilding(var game : GameData; loc : Point2D; terrainType : TerrainType);
	var
		idx: Integer;
		pos : Point2D;
	begin
	 	// Make room for new building
		SetLength(game.buildings, Length(game.buildings) + 1);
		idx := High(game.buildings);

		// Residential
		if terrainType = TERRAIN_ZONE then
		begin
			if Random(100) > 50 then
				game.buildings[idx].sprite := CreateSprite('Building', BitmapNamed('House01'))
			else
				game.buildings[idx].sprite := CreateSprite('Building', BitmapNamed('House02'));

			PlaySoundEffect('BuildRes');
			CreateCitizens(game, loc, 5);
			game.buildings[idx].buildingType := RES_SMALL;
		end;

		// Commercial
		if terrainType = TERRAIN_COMZONE then
		begin
			PlaySoundEffect('BuildCom');
			game.buildings[idx].sprite := CreateSprite('Building', BitmapNamed('ShopBL01'));
			game.buildings[idx].buildingType := COM_SMALL;
		end;

		// Water
		if terrainType = TERRAIN_PLACEWATER then
		begin
			PlaySoundEffect('BuildWater');
			game.buildings[idx].sprite := CreateSprite('Building', BitmapNamed('WaterPump'));
			game.buildings[idx].buildingType := COM_SMALL;
			SetLength(game.expenses, Length(game.expenses) + 1);
			game.expenses[High(game.expenses)] := 1000;
		end;

		// Power
		if terrainType = TERRAIN_POWER then
		begin
			PlaySoundEffect('BuildPower');
			game.buildings[idx].sprite := CreateSprite('Building', BitmapNamed('PowerPlant'));
			game.buildings[idx].buildingType := POWER;
			game.power += 10000;
			SetLength(game.expenses, Length(game.expenses) + 1);
			game.expenses[High(game.expenses)] := 2000;
		end;

		// Move the sprite...
		game.buildings[idx].loc := loc;
		pos := WorldBuilding(loc, game.buildings[idx].sprite);
		MoveSpriteTo(game.buildings[idx].sprite,Round(pos.x), Round(pos.y));
		UpdateSprite(game.buildings[idx].sprite);

		// Start the building timer
		game.buildings[idx].timer := CreateTimer();
		StartTimer(game.buildings[idx].timer);

		// Charge the city bank account 
		game.money -= 500;

		// Assign the building to the map tile
		game.map[Round(loc.x), Round(loc.y)].terrainType := TERRAIN_BUILDING;
		game.map[Round(loc.x), Round(loc.y)].hasBuilding := true;
		game.map[Round(loc.x), Round(loc.y)].building := game.buildings[idx];

		DebugMsg('Building: New building spawned at ' + PointToString(pos));
	end;

	// Gets all citizens that belong to a building
	function GetCitizensForBuilding(var game : GameData; loc : Point2D) : Citizens;
	var
		i: Integer;
	begin
		for i := 0 to High(game.citizens) do 
		begin
			if VectorsEqual(game.citizens[i].home, loc) then 
			begin
				SetLength(result, Length(result) + 1);
				result[High(result)] := game.citizens[i];
			end;
		end;
	end;

end.
