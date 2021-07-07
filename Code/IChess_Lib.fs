namespace ChessLib

open System

open DataTypes
open DataTypes.WeightsPaths
open DataTypes.VariablesForIChess
open ServiceCode.JSON_IO
open ServiceCode.Service
open ServiceCode.ChessFunc
open AI.Computing

module BvsB =
    let get_move (color: bool) (count: byte) (num: byte) = 
        let figures = readFromJSON<FigureData[]> pos_path
        let pos_info = create_position_info figures
        let weights_rnn = readFromJSON<float[][][]> (weights_rnn_path num)
        let pos_val = 
            let mas = readFromJSON<float[][]> (pos_val_path num)
            create_2DArray mas 8
        let king_num = if color then 4 else 20 
        //Проверка на трансформированных пешек
        for i in 0..31 do 
            if figures.[i].figure = Figure.Pawn && figures.[i].state = MoveFeature.Transform then 
                correct_mas_on_Tpawn_w weights_rnn pos_val num (int figures.[i].figure - 1) i
    
        let n1_o = neurons_1_output [| for i in 0..31 -> figures.[i].status |] weights_rnn.[0]
        let n2_o = neurons_2_output pos_val n1_o [| for i in 0..31 -> figures.[i].position |]
        let n3_o = neurons_3_output n2_o weights_rnn.[1] 
        let n4_o = neurons_4_output pos_val figures pos_info weights_rnn (not color) n1_o n3_o num
    
        if n4_o.Length > 0 then 
            let best_move = 
                let moves = Array.map (fun i -> try_to_win 1uy figures weights_rnn pos_val i pos_info color num) n4_o
                let val_mas = [|for i in moves -> fst i|]
                let id = Array.IndexOf(val_mas, Array.max(val_mas))
                (fst moves.[id] |> a_func_ht, snd moves.[id])
            let fig_id = snd (snd best_move)
            let new_figures = change_figures (Array.copy figures) (snd best_move)
            let chah_iden = 
                let e_king_num = if color then 20 else 4
                if check_shah (create_position_info new_figures) new_figures.[e_king_num].position color then
                    Array.set new_figures e_king_num { new_figures.[e_king_num] with state = MoveFeature.Check }
                    true
                else false
            writeToJson pos_path new_figures
    
            ( change_log (fst (snd best_move)) figures.[fig_id].figure figures.[fig_id].position king_num count color chah_iden, snd best_move )
        else 
            if check_shah (create_position_info figures) figures.[king_num].position (not color) then
                Array.set figures king_num { figures.[king_num] with state = MoveFeature.Mate }
            if figures.[king_num].state = MoveFeature.Mate then
                (PartyState.Dead, (MoveInfo.Default, -1))
            else 
                (PartyState.Draw, (MoveInfo.Default, -1))
    //Функция, управляющая партией
    let rec bvb_game (state: PartyState) (whose_move: bool) (count: byte) ((num_1, num_2): (byte * byte)) =
        match state with
        | PartyState.Play -> 
            let (iden, (move_info, id)) = get_move whose_move count (if whose_move then num_1 else num_2)
            let count_next = if whose_move then count else count + 1uy
            (count_next, byte iden, (move_info, id))
        | PartyState.Dead -> 
            let mas = readFromJSON<string[]> move_log_path
            Array.set mas (mas.Length - 1) (mas.[mas.Length - 1] + "#")
            writeToJson move_log_path mas
            (count, 1uy, (MoveInfo.Default, -1))
        | PartyState.Draw -> 
            let mas = readFromJSON<string[]> move_log_path
            Array.set mas (mas.Length - 1) (mas.[mas.Length - 1] + " 1/2-1/2")
            writeToJson move_log_path mas
            (count, 2uy, (MoveInfo.Default, -1))
        | _ -> 
            Console.Write("Error! -> game")
            Environment.Exit(0)
            (count, 1uy, (MoveInfo.Default, -1))
    
module PvsB = 
    //Обновить параметры
    let init_game () = 
        refresh_pos()
        weights_rnn_pvb <- readFromJSON<float[][][]> (weights_rnn_path bot_num)
        pos_val_pvb <- 
            let mas = readFromJSON<float[][]> (pos_val_path bot_num)
            create_2DArray mas 8
        b_move_info_pvb <- MoveInfo.Default
        b_move_id_pvb <- 0
        mid_error_pvb <- 0.0
    let get_fig_moves (e_color: bool) (fig_id: int) =
        let figures = readFromJSON<FigureData[]> pos_path
        define_move_list (create_position_info figures) figures fig_id e_color

    let get_move_pvb (color: bool) (count: byte) = 
        let figures = readFromJSON<FigureData[]> pos_path
        let pos_info = create_position_info figures
        let king_num = if color then 4 else 20

        //Проверка на трансформированных пешек
        for i in 0..31 do 
            if figures.[i].figure = Figure.Pawn && figures.[i].state = MoveFeature.Transform then 
                correct_mas_on_Tpawn_w weights_rnn_pvb pos_val_pvb bot_num (int figures.[i].figure - 1) i

        let n1_o = neurons_1_output [| for i in 0..31 -> figures.[i].status |] weights_rnn_pvb.[0]
        let n2_o = neurons_2_output pos_val_pvb n1_o [| for i in 0..31 -> figures.[i].position |]
        let n3_o = neurons_3_output n2_o weights_rnn_pvb.[1] 
        let n4_o = neurons_4_output pos_val_pvb figures pos_info weights_rnn_pvb (not color) n1_o n3_o bot_num

        if n4_o.Length > 0 then 
            let best_move = 
                let moves = Array.map (fun i -> try_to_win 1uy figures weights_rnn_pvb pos_val_pvb i pos_info color bot_num) n4_o
                let val_mas = [|for i in moves -> fst i|]
                let id = Array.IndexOf(val_mas, Array.max(val_mas))
                (fst moves.[id] |> a_func_ht, snd moves.[id])
            b_move_info_pvb <- fst (snd best_move)
            b_move_id_pvb <- snd (snd best_move)
            let fig_id = snd (snd best_move)
            let new_figures = change_figures (Array.copy figures) (snd best_move)
            let chah_iden = 
                let e_king_num = if color then 20 else 4
                if check_shah (create_position_info new_figures) new_figures.[e_king_num].position color then
                    Array.set new_figures e_king_num { new_figures.[e_king_num] with state = MoveFeature.Check }
                    true
                else false
            writeToJson pos_path new_figures

            change_log (fst (snd best_move)) figures.[fig_id].figure figures.[fig_id].position king_num count color chah_iden
        else 
            if check_shah (create_position_info figures) figures.[king_num].position (not color) then
                Array.set figures king_num { figures.[king_num] with state = MoveFeature.Mate }
            if figures.[king_num].state = MoveFeature.Mate then
                PartyState.Dead
            else 
                PartyState.Draw
    let calculate_pvb_statistics (color: bool) (count: byte) = 
        let error = mid_error_pvb / (if color then float (count - 1uy) else float count)
        let er_mas = 
            let mas = readFromJSON<float[]> pvb_errors_path
            [| 
                for i in mas -> i 
                yield error 
            |]
        writeToJson pvb_errors_path er_mas

    let rec pvb_game (move_info: MoveInfo) (id: int) (whose_move: bool) (count: byte) =
        //Обрабатываем ход игрока
        let state = 
            if not (id = -1) then
                let figures = readFromJSON<FigureData[]> pos_path
                let new_figures = change_figures (Array.copy figures) (move_info, id)
                let chah_iden = 
                    let e_king_num = if whose_move then 4 else 20
                    if check_shah (create_position_info new_figures) new_figures.[e_king_num].position (not whose_move) then
                        Array.set new_figures e_king_num { new_figures.[e_king_num] with state = MoveFeature.Check }
                        true
                    else false
                writeToJson pos_path new_figures
                change_log move_info figures.[id].figure figures.[id].position (if whose_move then 20 else 4) count (not whose_move) chah_iden
            else PartyState.Play
        let count_next = if whose_move then count else count + 1uy
        //Передача хода боту
        match state with
        | PartyState.Play -> 
            let iden = get_move_pvb whose_move count
            (count_next, (byte)iden, (b_move_info_pvb, b_move_id_pvb))
        | PartyState.Dead -> 
            calculate_pvb_statistics whose_move count
            (count_next, 1uy, (MoveInfo.Default, 32))
        | PartyState.Draw -> 
            calculate_pvb_statistics whose_move count
            (count_next, 2uy, (MoveInfo.Default, 32))
        | _ -> 
            System.Console.Write("Error! -> game")
            (count_next, 1uy, (MoveInfo.Default, 32))