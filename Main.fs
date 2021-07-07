open System

open DataTypes
open DataTypes.Variables
open DataTypes.WeightsPaths
open ServiceCode.Service
open ServiceCode.JSON_IO
open ServiceCode.ChessFunc
open AI.Computing
open AI.Traininig
 

//Функция, управляющая ходом
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
    add_record figures (Array.sum n3_o |> a_func_ht) (Convert.ToInt32(not color)) n1_o n2_o

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

        change_log (fst (snd best_move)) figures.[fig_id].figure figures.[fig_id].position king_num count color chah_iden
    else 
        if check_shah (create_position_info figures) figures.[king_num].position (not color) then
            Array.set figures king_num { figures.[king_num] with state = MoveFeature.Mate }
        add_record figures (Array.sum n3_o |> a_func_ht) (Convert.ToInt32(not color)) n1_o n2_o
        if figures.[king_num].state = MoveFeature.Mate then
            PartyState.Dead
        else 
            PartyState.Draw
//Функция, управляющая партией
let rec bvb_game (state: PartyState) (whose_move: bool) (count: byte) ((num_1, num_2): (byte * byte)) =
    match state with
    | PartyState.Play ->
        let iden = get_move whose_move count (if whose_move then num_1 else num_2)
        let count_next = if whose_move then count else count + 1uy
        bvb_game iden (not whose_move) count_next (num_1, num_2)
    | PartyState.Dead -> 
        let mas = readFromJSON<string[]> move_log_path
        Array.set mas (mas.Length - 1) (mas.[mas.Length - 1] + "#")
        writeToJson move_log_path mas
        let delta_arr = train ()
        calculate_bvb_statistics whose_move count (int num_1 - 1, int num_2 - 1)
        refresh_weights delta_arr.[0] num_1 0 
        refresh_weights delta_arr.[1] num_2 1 
        let name = if whose_move then $"AI_{num_2}" else $"AI_{num_1}"
        Console.WriteLine($"Король беспомощен, победа за {name}! Ход - {count}.")
    | PartyState.Draw -> 
        let mas = readFromJSON<string[]> move_log_path
        Array.set mas (mas.Length - 1) (mas.[mas.Length - 1] + " 1/2-1/2")
        writeToJson move_log_path mas
        let delta_arr = train ()
        calculate_bvb_statistics whose_move count (int num_1 - 1, int num_2 - 1)
        refresh_weights delta_arr.[0] num_1 0 
        refresh_weights delta_arr.[1] num_2 1 
        Console.WriteLine($"Никто не хотел умирать. Ничья! Ход - {count}.")
    | _ -> 
        Console.Write("Error! -> game")
        Environment.Exit(0)

let neuroFunc() = 
    for j = 0 to 0 do
        printfn "Эпоха - %A." j
        epsilon <- Math.Exp(-(float j) / eps_count)
        let pair_mas = (byte (gen.Next(2)) + 1uy, byte (gen.Next(2)) + 1uy)
        bvb_game PartyState.Play true 1uy pair_mas
        refresh_pos()
        refresh_mut_param()
                                                                    
[<EntryPoint>]
//start ()
refresh_pos()
neuroFunc()
