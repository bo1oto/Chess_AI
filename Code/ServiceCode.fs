namespace ServiceCode

open DataTypes
open DataTypes.Variables
open DataTypes.WeightsPaths
open System

module JSON_IO = 
    open System.IO
    open FSharp.Json
    //JSON сериализация/десериализация
    let writeToJson (path: string) (obj: 'a) : unit =
        use sw = new StreamWriter(path)
        let txt = Json.serialize obj
        sw.Write(txt)
    let readFromJSON<'T> (path: string) =
        use sr = new StreamReader(path)
        let text = sr.ReadToEnd()
        Json.deserialize<'T> text

module Service =
    open JSON_IO
    let gen = new Random()
    let inline generator() = 
        if gen.Next(0,2) = 1 then
            -gen.NextDouble()
        else
            gen.NextDouble()
    let inline create_2DArray (initmas: 'a[][]) (size: int) = 
        [| for i in 0..(initmas.Length - 1) -> Array2D.init size size (fun w j -> initmas.[i].[w * size + j])|]
    let inline create_position_info (figures : FigureData[]) =
        let mas = 
            [|
                for i in 0..7 ->
                    [|
                        for j in 0..7 do
                            { 
                                PositionInfo.figure =
                                    let x = Array.tryFind (fun fig -> fig.status && fig.position = (i, j)) figures
                                    match x with
                                    | Some(x) -> x.figure
                                    | None -> Figure.Empty
                                PositionInfo.color = 
                                    let x = Array.tryFindIndex (fun fig -> fig.status && fig.position = (i, j)) figures
                                    match x with
                                    | Some(x) -> x < 16
                                    | None -> false
                            }
                        
                    |]
            |]
        Array2D.init 8 8 (fun i j -> mas.[i].[j])
    let inline find_best_moves (val_mas : (float * (MoveInfo * int))[]) =
        let val_arr = 
            let mas = [|for i in val_mas -> fst i|]
            let sort_mas = Array.sortDescending mas
            sort_mas.[..best_move_count]
        Array.filter (fun x -> Array.contains (fst x) val_arr) val_mas
    //Начальные данные и веса
    let refresh_pos () = 
        let start_pos = 
            let mas = 
                [| 
                    [|
                        { FigureData.Default with position = (0, 0); figure = Figure.Rook; }
                        { FigureData.Default with position = (1, 0); figure = Figure.Knight; }
                        { FigureData.Default with position = (2, 0); figure = Figure.Bishop; }
                        { FigureData.Default with position = (3, 0); figure = Figure.Queen; }
                        { FigureData.Default with position = (4, 0); figure = Figure.King; }
                        { FigureData.Default with position = (5, 0); figure = Figure.Bishop; }
                        { FigureData.Default with position = (6, 0); figure = Figure.Knight }
                        { FigureData.Default with position = (7, 0); figure = Figure.Rook; }
                    |]
                    [| for i in 0 .. 7 -> { FigureData.Default with position = (i, 1); figure = Figure.Pawn; } |] 
                    [|
                        { FigureData.Default with position = (0, 7); figure = Figure.Rook; }
                        { FigureData.Default with position = (1, 7); figure = Figure.Knight; }
                        { FigureData.Default with position = (2, 7); figure = Figure.Bishop; }
                        { FigureData.Default with position = (3, 7); figure = Figure.Queen; }
                        { FigureData.Default with position = (4, 7); figure = Figure.King; }
                        { FigureData.Default with position = (5, 7); figure = Figure.Bishop; }
                        { FigureData.Default with position = (6, 7); figure = Figure.Knight; }
                        { FigureData.Default with position = (7, 7); figure = Figure.Rook; }
                    |]; 
                    [| for i in 0 .. 7 -> { FigureData.Default with position = (i, 6); figure = Figure.Pawn; } |] 
                |]
            [|
                for i in 0..3 do 
                    for j in 0..7 -> 
                        mas.[i].[j]
            |]
        writeToJson pos_path start_pos
        writeToJson move_log_path [||]
    let start () = 
        // 1 - горизонталь, 2 - вертикаль
        refresh_pos()
        writeToJson errors_path [| for _ in 1..2 -> [|0.0|]|]
        writeToJson pvb_errors_path [|0.0|]
        for i in 1uy..2uy do
            let start_weights_rnn = 
                [|
                    [| 
                        for j in 1..32 ->
                            [| 
                                for w in 1..32 -> generator() 
                            |]
                    |]
                    [| 
                        for j in 1..32 ->
                            [| 
                                for w in 1..32 -> generator() 
                            |]
                    |]
                |]
            let (start_weights_rnn_d: float[][][]) = [| [| for _ in 0..31 -> Array.zeroCreate 32|]; [| for _ in 0..31 ->  Array.zeroCreate 32 |] |]
            writeToJson (weights_rnn_path i) start_weights_rnn
            writeToJson (weights_rnn_d_path i) start_weights_rnn_d
            let start_pos_val =
                [|
                    for _ in 0..31 ->
                        [| for _ in 0..63 -> generator () |]
                |]
            let (start_pos_val_d: float[][]) = [| for _ in 0..31 -> Array.zeroCreate 64 |]
            writeToJson (pos_val_path i) start_pos_val
            writeToJson (pos_val_d_path i) start_pos_val_d
            let start_pawn_transform_w = 
                [| 
                    [| 
                        for _ in 1..4 ->
                            [| 
                                for _ in 1..16 ->
                                    [| 
                                        for _ in 1..32 -> generator() 
                                    |]
                            |]
                    |]
                    [| 
                        for _ in 1..4 ->
                            [| 
                                for _ in 1..16 ->
                                    [| 
                                        for _ in 1..32 -> generator() 
                                    |]
                            |]
                    |]
                |]
            let (start_pawn_transform_w_d: float[][][][]) = 
                [| 
                    [| 
                        for _ in 1..4 ->
                            [| 
                                for _ in 1..16 -> Array.zeroCreate 32
                            |]
                    |]
                    [| 
                        for _ in 1..4 ->
                            [| 
                                for _ in 1..16 -> Array.zeroCreate 32
                            |]
                    |]
                |]
            writeToJson (pt_weights_rnn_path i) start_pawn_transform_w
            writeToJson (pt_weights_rnn_d_path i) start_pawn_transform_w_d
            let start_pawn_transform_pos_val = 
                [| 
                    for _ in 1..4 -> 
                        [| 
                            for _ in 1..16 -> 
                                [| 
                                    for _ in 0..63 -> generator ()
                                |] 
                        |] 
                |]
            let (start_pawn_transform_pos_val_d: float[][][]) = 
                [| 
                    for _ in 1..4 -> 
                        [| 
                            for _ in 1..16 -> Array.zeroCreate 64
                        |] 
                |]
            writeToJson (pt_pos_val_path i) start_pawn_transform_pos_val
            writeToJson (pt_pos_val_d_path i) start_pawn_transform_pos_val_d
    //Поправка на трансформированных пешек
    let correct_mas_on_Tpawn_w (weights_rnn: float[][][]) (pos_val: float[,][]) (num: byte) (fig_id: int) (id: int) = 
        let tp_mas_w = readFromJSON<float[][][][]> (pt_weights_rnn_path num)
        let tp_mas_pv = 
            let mas = readFromJSON<float[][][]> (pt_pos_val_path num)
            [| 
                for i in 0..3 ->
                    create_2DArray mas.[i] 8
            |]
        let p_id = id % 16
        Array.set weights_rnn.[0] id tp_mas_w.[0].[fig_id].[p_id]
        Array.set weights_rnn.[1] id tp_mas_w.[1].[fig_id].[p_id]
        Array.set pos_val id tp_mas_pv.[fig_id].[p_id]
    //тут ещё можно выровнять при:
    //1. рокировке у белого
    //2. x - занимает 1 символ, -> - два
    //3. значок ничьи, если она на ходу чёрного может смотреться не очень (хотя хз)
    let change_log (move_info: MoveInfo) (figure: Figure) (position: (int * int)) (king_num: int) (count: byte) (color: bool) (isChah: bool) = 
        let sign = if move_info.id <> -1y then "x" else "->"
        let (f_tag, post_figure) = 
            let tag = 
                match figure with
                | Figure.Pawn -> ' '
                | Figure.Knight-> 'N'
                | Figure.Bishop -> 'B'
                | Figure.Queen -> 'Q'
                | Figure.Rook -> 'R'
                | Figure.King -> 'K'
                | _ -> '!'
            if move_info.fig_num > 0y && tag <> 'K' then (' ', tag) else (tag, ' ')
        let chah_iden = if isChah then '+' else ' '
        let log = 
            let str_move = 
                let wth_m = 
                    if not (move_info.fig_num = 0y) then
                        $"{f_tag}{char (fst position + 97)}{snd position + 1} {sign} " + 
                        $"{char (fst move_info.move + 97)}{snd move_info.move + 1}{post_figure}{chah_iden}"
                    elif int (move_info.id) < king_num then 
                        "0-0-0" else "0-0"
                let counter = if color then $"{count}.   " else " "
                $"{counter.[..4]}{wth_m}"
            let mas = (readFromJSON<string[]> move_log_path)
            if not color then 
                Array.set mas (int (count - 1uy)) (mas.[int (count - 1uy)] + str_move)
                mas
            else 
                let plus_one_mas = 
                    [| 
                        for i in mas -> i 
                        yield str_move 
                    |]
                plus_one_mas
        writeToJson move_log_path log
        let id = log.Length
        if count > 10uy && not color 
          && log.[id - 1].[4..] = log.[id - 3].[4..] && log.[id - 2].[4..] = log.[id - 4].[4..] 
          && log.[id - 3].[4..] = log.[id - 5].[4..] && log.[id - 4].[4..] = log.[id - 6].[4..]
          && log.[id - 5].[4..] = log.[id - 7].[4..] && log.[id - 6].[4..] = log.[id - 8].[4..] 
          || log.Length > 150 then
              PartyState.Draw else PartyState.Play
    

module ChessFunc =
    //Функции измененения состояния
    let change_pos_info (new_pos_info: PositionInfo[,]) ((x, y): (int * int)) (info: MoveInfo) = 
        if new_pos_info.[x, y].figure = Figure.Pawn then
            if new_pos_info.[fst info.move, snd info.move].figure = Figure.Empty && (fst info.move) <> x then
                let move = if new_pos_info.[x, y].color then -1 else 1 
                Array2D.set new_pos_info (fst info.move) ((snd info.move) + move) { figure = Figure.Empty; color = false }
            if info.fig_num > 0y then 
                Array2D.set new_pos_info x y { new_pos_info.[x, y] with figure = LanguagePrimitives.EnumOfValue<byte, Figure>(byte info.fig_num) }
        if info.fig_num = 0y then
            if fst info.move - x > 0 then
                Array2D.set new_pos_info 5 y new_pos_info.[7, y]
                Array2D.set new_pos_info 7 y { figure = Figure.Empty; color = false }
            else
                Array2D.set new_pos_info 3 y new_pos_info.[0, y]
                Array2D.set new_pos_info 0 y { figure = Figure.Empty; color = false }
        Array2D.set new_pos_info (fst info.move) (snd info.move) new_pos_info.[x, y]
        Array2D.set new_pos_info x y { figure = Figure.Empty; color = false }
        new_pos_info
    let change_figures (figure_mas: FigureData[]) ((info, id): (MoveInfo * int)) = 
        Array.set figure_mas id { figure_mas.[id] with state = MoveFeature.Simple }//Тут прикол в том, что из за последующих действий нельзя менять позицию сразу
        if info.id <> -1y then
            Array.set figure_mas (int info.id) { figure_mas.[int info.id] with status = false; }
        if figure_mas.[id].figure = Figure.Pawn && Math.Abs ((snd info.move) - (snd figure_mas.[id].position)) = 2 then
            Array.set figure_mas id { figure_mas.[id] with state = MoveFeature.ForwardMove }
        if info.fig_num <> -1y then
            if figure_mas.[id].figure = Figure.King then//не понятно, нужно ли вписывать рокировку
                let (k_rook, q_rook) = if id = 20 then (16, 23) else (0, 7)
                if fst info.move - fst figure_mas.[id].position > 0  then
                    Array.set figure_mas q_rook { figure_mas.[q_rook] with position = (5, snd figure_mas.[id].position); state = MoveFeature.Castling }
                else 
                    Array.set figure_mas k_rook { figure_mas.[k_rook] with position = (3, snd figure_mas.[id].position); state = MoveFeature.Castling }
            if figure_mas.[id].figure = Figure.Pawn then
                Array.set figure_mas id { figure_mas.[id] with figure = LanguagePrimitives.EnumOfValue<byte, Figure>(byte info.fig_num); state = MoveFeature.Transform }
        Array.set figure_mas id { figure_mas.[id] with position = info.move }
        let (st, en) = if id < 16 then (16, 31) else (0, 15)
        for i in st..en do
            if figure_mas.[i].state = MoveFeature.ForwardMove then
                Array.set figure_mas i { figure_mas.[i] with state = MoveFeature.Simple }
        figure_mas
    //Функции определения возможных ходов    
    let rec check_fun (pos: (int * int)) (pos_info: PositionInfo[,]) (e_color: bool) (iteration: int) 
        (move_func: int -> int-> (int * int)) (condition: Figure -> int -> bool) = 
        let (x, y) = move_func (fst pos) (snd pos)
        let inRange = not (x = 8 || x = -1 || y = -1 || y = 8)
        match inRange with
        | true ->
            if pos_info.[x, y].figure <> Figure.Empty then
                if pos_info.[x, y].color = e_color && condition pos_info.[x, y].figure iteration then 
                    true 
                else false
            else check_fun (x, y) pos_info e_color (iteration + 1) move_func condition
        | false -> false
    let check_shah (pos_info : PositionInfo[,]) ((h, v) : (int * int)) (enemy_color : bool) = 
        
            let diag_cond_f = fun fig i -> fig = Figure.King || fig = Figure.Bishop || fig = Figure.Queen || fig = Figure.Pawn && i = 0
            let diag_cond_b = fun fig i -> fig = Figure.King || fig = Figure.Bishop || fig = Figure.Queen
            let direct_cond = fun fig i -> fig = Figure.King || fig = Figure.Rook || fig = Figure.Queen
            let knight_cond (x: int) (y: int) (pos_info: PositionInfo[,]) (e_color: bool) =
                let pos_mas = 
                    [|
                        if x - 1 > -1 && y + 2 < 8 then (x - 1, y + 2)
                        if x + 1 < 8 && y + 2 < 8 then (x + 1, y + 2)
                        if x + 2 < 8 && y + 1 < 8 then (x + 2, y + 1)
                        if x + 2 < 8 && y - 1 > -1 then (x + 2, y - 1)
                        if x + 1 < 8 && y - 2 > -1 then (x + 1, y - 2)
                        if x - 1 > -1 && y - 2 > -1 then (x - 1, y - 2)
                        if x - 2 > -1 && y - 1 > -1 then (x - 2, y - 1)
                        if x - 2 > -1 && y + 1 < 8 then (x - 2, y + 1)
                    |]
                let mutable iden = false
                for i in pos_mas do
                    if pos_info.[fst i, snd i].figure = Figure.Knight && pos_info.[fst i, snd i].color = e_color then
                            iden <- true
                iden
    
            let move = if enemy_color then -1 else 1
        
            let fix_cf = check_fun (h, v) pos_info enemy_color 0
            fix_cf (fun x y -> (x + 1, y)) (direct_cond) ||
            fix_cf (fun x y -> (x - 1, y)) (direct_cond) ||
            fix_cf (fun x y -> (x, y + 1)) (direct_cond) ||
            fix_cf (fun x y -> (x, y - 1)) (direct_cond) ||
            
            fix_cf (fun x y -> (x + 1, y + move)) (diag_cond_f) ||
            fix_cf (fun x y -> (x - 1, y + move)) (diag_cond_f) ||
            fix_cf (fun x y -> (x + 1, y - move)) (diag_cond_b) ||
            fix_cf (fun x y -> (x - 1, y - move)) (diag_cond_b) ||
            knight_cond (int h) (int v) pos_info enemy_color
    let rec define_move_list (pos_info: PositionInfo[,]) (figures: FigureData[]) (fig_id: int) (enemy_color: bool) = 
        let mutable (move_list: list<MoveInfo>) = []
        let inline fix_insert x = x :: move_list
        let position = figures.[fig_id].position
        let king_num = if enemy_color then 20 else 4
        let inline find_id (move: int * int) = sbyte (Array.findIndex (fun fig -> fig.status && fig.position = move) figures)
        let rec fill_move_list ((h,v): (int * int)) (move_func: int -> int -> (int * int)) =
            let (x,y) = move_func h v
            let inRange = not (x = 8 || x = -1 || y = -1 || y = 8)
            if inRange then
                if pos_info.[x, y].figure <> Figure.Empty then 
                    if pos_info.[x, y].color = enemy_color then
                        move_list <- 
                            { MoveInfo.Default with 
                                move = (x,y)
                                id = sbyte (Array.findIndex (fun fig -> fig.position = (x, y) && fig.status) figures)
                            } :: move_list
                else 
                    move_list <- { MoveInfo.Default with move = (x,y) } :: move_list
                    fill_move_list (x,y) move_func 
        let short_fill_move_list = fill_move_list position
        match figures.[fig_id].figure with
        | Figure.Pawn ->
            let move_direction = if enemy_color then -1 else 1
            //Ход вперёд
            if pos_info.[fst position, snd position + move_direction].figure = Figure.Empty then
                if figures.[fig_id].state = MoveFeature.Sleep && pos_info.[fst position, snd position + (2 * move_direction)].figure = Figure.Empty then
                    move_list <- fix_insert { MoveInfo.Default with move = (fst position, snd position + (2 * move_direction)); }
                if enemy_color && snd position = 1 || not enemy_color && snd position = 6 then 
                    for i in 1y..4y do
                        move_list <- fix_insert { MoveInfo.Default with move = (fst position, snd position + move_direction); fig_num = i }
                else
                    move_list <- fix_insert { MoveInfo.Default with move = (fst position, snd position + move_direction); }
            //Взятие
            if fst position - 1 > -1 then
                let move = (fst position - 1, snd position + move_direction)
                if pos_info.[fst move, snd move].figure <> Figure.Empty 
                    && pos_info.[fst move, snd move].color = enemy_color then
                    let id = find_id move
                    if enemy_color && snd position = 1 || not enemy_color && snd position = 6 then 
                        for i in 1y..4y do
                            move_list <- fix_insert { move = figures.[int id].position; id = id; fig_num = i }
                    else move_list <- fix_insert { MoveInfo.Default with move = figures.[int id].position; id = id }
                //На проходе
                if pos_info.[fst position - 1, snd position].figure = Figure.Pawn
                    && pos_info.[fst position - 1, snd position].color = enemy_color then
                    let id = find_id (fst position - 1, snd position)
                    if figures.[int id].state = MoveFeature.ForwardMove then
                        move_list <- fix_insert { MoveInfo.Default with move = move; id = id }
            if fst position + 1 < 8 then 
                let move = (fst position + 1, snd position + move_direction)
                if pos_info.[fst move, snd move].figure <> Figure.Empty 
                    && pos_info.[fst move, snd move].color = enemy_color then
                    let id = find_id move
                    if enemy_color && snd move = 0 || not enemy_color && snd move = 7 then 
                        for i in 1y..4y do
                            move_list <- fix_insert { move = figures.[int id].position; id = id; fig_num = i }
                    else move_list <- fix_insert { MoveInfo.Default with move = figures.[int id].position; id = id }
                //На проходе
                if pos_info.[fst position + 1, snd position].figure = Figure.Pawn 
                    && pos_info.[fst position + 1, snd position].color = enemy_color then
                    let id = find_id (fst position + 1, snd position)
                    if figures.[int id].state = MoveFeature.ForwardMove then
                        move_list <- fix_insert { MoveInfo.Default with move = move; id = id }
        | Figure.Rook -> 
            short_fill_move_list (fun x y -> (x + 1, y))
            short_fill_move_list (fun x y -> (x - 1, y))
            short_fill_move_list (fun x y -> (x, y + 1))
            short_fill_move_list (fun x y -> (x, y - 1))
        | Figure.Knight ->
            let move_arr = 
                [|
                    if fst position - 1 > -1 && snd position + 2 < 8 then (fst position - 1, snd position + 2)
                    if fst position + 1 < 8 && snd position + 2 < 8 then (fst position + 1, snd position + 2)
                    if fst position + 2 < 8 && snd position + 1 < 8 then (fst position + 2, snd position + 1)
                    if fst position + 2 < 8 && snd position - 1 > -1 then (fst position + 2, snd position - 1)
                    if fst position + 1 < 8 && snd position - 2 > -1 then (fst position + 1, snd position - 2)
                    if fst position - 1 > -1 && snd position - 2 > -1 then (fst position - 1, snd position - 2)
                    if fst position - 2 > -1 && snd position - 1 > -1 then (fst position - 2, snd position - 1)
                    if fst position - 2 > -1 && snd position + 1 < 8 then (fst position - 2, snd position + 1)
                |]
            for move in move_arr do
                if pos_info.[fst move, snd move].figure <> Figure.Empty then
                    if pos_info.[fst move, snd move].color = enemy_color then
                        move_list <- fix_insert { MoveInfo.Default with move = move; id = find_id move }
                else move_list <- fix_insert { MoveInfo.Default with move = move }
        | Figure.Bishop ->
            short_fill_move_list (fun x y -> (x + 1, y + 1))
            short_fill_move_list (fun x y -> (x - 1, y + 1))
            short_fill_move_list (fun x y -> (x + 1, y - 1))
            short_fill_move_list (fun x y -> (x - 1, y - 1))
        | Figure.Queen -> 
            short_fill_move_list (fun x y -> (x + 1, y))
            short_fill_move_list (fun x y -> (x - 1, y))
            short_fill_move_list (fun x y -> (x, y + 1))
            short_fill_move_list (fun x y -> (x, y - 1))
            short_fill_move_list (fun x y -> (x + 1, y + 1))
            short_fill_move_list (fun x y -> (x - 1, y + 1))
            short_fill_move_list (fun x y -> (x + 1, y - 1))
            short_fill_move_list (fun x y -> (x - 1, y - 1))
        | Figure.King ->
            let move_arr = 
                [|
                    if fst position - 1 > -1 && snd position - 1 > -1 then (fst position - 1, snd position - 1)
                    if fst position - 1 > -1 then (fst position - 1, snd position)
                    if fst position - 1 > -1 && snd position + 1 < 8 then (fst position - 1, snd position + 1)
                    if snd position + 1 < 8 then (fst position, snd position + 1)
                    if fst position + 1 < 8 && snd position + 1 < 8 then (fst position + 1, snd position + 1)
                    if fst position + 1 < 8 then (fst position + 1, snd position)
                    if fst position + 1 < 8 && snd position - 1 > -1 then (fst position + 1, snd position - 1)
                    if snd position - 1 > -1 then (fst position, snd position - 1)
                |]
            for move in move_arr do
                if pos_info.[fst move, snd move].figure <> Figure.Empty then
                    if pos_info.[fst move, snd move].color = enemy_color then
                        move_list <- fix_insert { MoveInfo.Default with move = move; id = find_id move }
                else move_list <- fix_insert { MoveInfo.Default with move = move }
            //проверка на рокировку
            if figures.[fig_id].state = MoveFeature.Sleep && not (check_shah pos_info (fst position, snd position) enemy_color) then
                let s_pos = if enemy_color then 7 else 0
                let (r_num_1, r_num_2) = if enemy_color then (16, 23) else (0, 7)
                if figures.[r_num_1].state = MoveFeature.Sleep then 
                    if pos_info.[2, s_pos].figure = Figure.Empty && pos_info.[3, s_pos].figure = Figure.Empty 
                        && not (check_shah pos_info (2, s_pos) enemy_color)
                        && pos_info.[0, s_pos].figure = Figure.Rook && pos_info.[0, s_pos].color = not enemy_color then
                        move_list <- fix_insert { MoveInfo.Default with move = (2, s_pos); fig_num = 0y }//номер ладьи
                if figures.[r_num_2].state = MoveFeature.Sleep then 
                    if pos_info.[5, s_pos].figure = Figure.Empty && pos_info.[6, s_pos].figure = Figure.Empty
                        && not (check_shah pos_info (5, s_pos) enemy_color)
                        && pos_info.[7, s_pos].figure = Figure.Rook && pos_info.[7, s_pos].color = not enemy_color then
                        move_list <- fix_insert { MoveInfo.Default with move = (6, s_pos); fig_num = 0y }//номер ладьи
        | _ -> 
            Console.WriteLine("Error -> define_move_list!")
            Environment.Exit(0)
        let filter_cond move_info = 
            let king_position = if figures.[fig_id].figure = Figure.King then move_info.move else figures.[king_num].position
            check_shah (change_pos_info (Array2D.copy pos_info) position move_info) (fst king_position, snd king_position) enemy_color
        List.filter(fun x -> not (filter_cond x)) move_list
