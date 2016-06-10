unit gGUI;
//=============================================================================
// gGUI.pas
//=============================================================================
//
// Responsible for drawing and handling user input for the main menu
//
//=============================================================================


interface
	uses SwinGame, sgTypes, gTypes;

	procedure DrawMainMenu(var game : GameData);
implementation
	procedure DrawMainMenu(var game : GameData);
	begin
		ClearScreen(ColorWhite);

		// Initial play/quit screen
		if game.menuStatus = MAIN_SCREEN then
		begin
			DrawBitmap('MainMenuBackground', 0, 0);
			if (MouseX() > 480) and (MouseY() > 320)  and (MouseX() < 800) and (MouseY() < 400) then
			begin
				DrawBitmap('MainMenuPlayBtnHover', 480, 320);

				if MouseClicked(LeftButton) then
				begin
					PlaySoundEffect('UIClick');
					StartReadingText(ColorBlack, 15, FontNamed('OpenSansSemiBold42'), 450, 325);
					game.menuStatus := CITY_NAME;
				end;
			end;

			if (MouseX() > 480) and (MouseY() > 430)  and (MouseX() < 800) and (MouseY() < 510) then
			begin
				DrawBitmap('MainMenuQuitBtnHover', 480, 428);

				if MouseClicked(LeftButton) then
				begin
					PlaySoundEffect('UIClick');
					game.guiStatus := SHUTDOWN;
				end;
			end;
		end;

		// City-name choosing screen
		if game.menuStatus = CITY_NAME then
		begin
			DrawBitmap('MainMenuName', 0, 0);

			if (MouseX() > 480) and (MouseY() > 430)  and (MouseX() < 800) and (MouseY() < 510) then
			begin
				DrawBitmap('MainMenuOkBtnHover', 480, 428);

				if (MouseClicked(LeftButton)) then
				begin
					PlaySoundEffect('UIClick');
					game.cityName := EndReadingText();
					game.guiStatus := IN_GAME;
					game.moneyTimer := CreateTimer();
					StartTimer(game.moneyTimer);
				end;
			end;

			if KeyTyped(ReturnKey) then
			begin
				game.cityName := EndReadingText();
				game.guiStatus := IN_GAME;
				game.moneyTimer := CreateTimer();
				StartTimer(game.moneyTimer);
			end;
		end;

		if DEBUG_MODE then
		begin
			DrawText('NOTE: GAME RUNNING IN DEBUG MODE', ColorBlack, 0,0);
		end;
		DrawText('101118713', ColorBlack, 'OpenSansSemiBold16', 0,680);
		DrawText('Music: Two Steps From Hell - Love You Forever', ColorBlack, 'OpenSansSemiBold16', 0,700);

		RefreshScreen();
	end;


end.
