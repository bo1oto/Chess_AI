namespace AI

open DataTypes
open DataTypes.WeightsPaths
open DataTypes.Variables
open ServiceCode.JSON_IO
open ServiceCode.Service
open ServiceCode.ChessFunc
open System

module Computing = 
    //Функция активации - гиперболический тангенс
    let inline a_func_ht x = (Math.Exp(x) - Math.Exp(-x)) / (Math.Exp(x) + Math.Exp(-x))
    let inline d_a_func_ht x = 1.0 - x**2.0

    //Первый слой
    //Выход первого слоя - value фигуры, исходя из имеющихся на поле фигур
    let neurons_1_output (mas_s : bool[]) (mas_w : float[][]) : float[] =  
        [|
            for i in 0..31 -> 
                [| 
                    for j in 0..31 -> mas_w.[i].[j] * Convert.ToDouble(mas_s.[j]) 
                |] 
                |> Seq.sum |> a_func_ht         
        |]
    //Второй слой
    //Выход второго слоя - value позиции фигуры, взвешенное по первому слою
    let neurons_2_output (pos_val : float[,][]) (n1_o : float[]) (fig_pos : (int * int)[]) =  
        [|
            for i in 0..31 ->
                n1_o.[i] * pos_val.[i].[fst fig_pos.[i], snd fig_pos.[i]] |> a_func_ht             
        |]
    //Третий слой
    //Итоговое value фигуры, исходя из value остальных фигур
    let neurons_3_output (n2_o : float[]) (mas_w : float[][]) =  
        [|
            for i in 0..31 ->
            [| 
                for j in 0..31 -> n2_o.[j] * mas_w.[i].[j]
            |]
            |> Seq.sum |> a_func_ht             
        |]
    //Четвёртый слой
    //Вычисляет разницу между состояниями до и после возможных ходов   
    let neurons_4_output (pos_val : float[,][]) (figure_mas : FigureData[]) (pos_info :PositionInfo[,]) (weights_rnn : float[][][])
        (enemy_color : bool) (n1_o: float[]) (n3_o: float[]) (num: byte) =  
        let (st, en) = if not enemy_color then (0, 15) else (16, 31)
        let mas =
            [|
                for i in st..en do
                    if figure_mas.[i].status then
                        let move_list = define_move_list pos_info figure_mas i enemy_color
                        if move_list.Length > 0 then
                            let (mas: float[]) = 
                                [| 
                                    for move in move_list do
                                        let new_figures = change_figures (Array.copy figure_mas) (move, i)
                                        let cr_mas_w = (Array.copy weights_rnn, Array.copy pos_val)
                                        if move.fig_num <> -1y && figure_mas.[i].figure <> Figure.King then
                                            correct_mas_on_Tpawn_w (fst cr_mas_w) (snd cr_mas_w) num (int move.fig_num - 1) i
                                        let n1_oo =  
                                            if move.id <> -1y || move.fig_num > 0y then
                                                neurons_1_output [| for e in 0..31 -> new_figures.[e].status |] (fst cr_mas_w).[0]
                                            else n1_o
                                        let n2_oo = neurons_2_output (snd cr_mas_w) n1_oo [| for i in new_figures -> i.position |]
                                        let n3_oo = neurons_3_output n2_oo (fst cr_mas_w).[1]
                                        Array.sum n3_oo - Array.sum n3_o//Получившийся - исходный
                                |]
                            [| for j in 0..(mas.Length - 1) -> (mas.[j], (move_list.[j], i)) |]
            |]
        let full_mas = 
            [| 
                for i in 0..(mas.Length - 1) do
                    for j in 0..(mas.[i].Length - 1) -> mas.[i].[j]
            |]
        let length = full_mas.Length
        if length > 0 then 
            if epsilon > gen.NextDouble() then
                [| full_mas.[gen.Next(0, length)] |]
            else
                if length > best_move_count then
                    find_best_moves full_mas
                else full_mas
        else [||]
    //Оценка хода с точки зрения будущих перспектив
    let rec try_to_win (cur_depth: byte) (figures: FigureData[]) (weights_rnn: float[][][]) (pos_val: float[,][]) 
        ((value, (info, id)): (float * (MoveInfo * int))) (pos_info :PositionInfo[,]) (e_color: bool) (num: byte) =
        if cur_depth < depth then//начинаем с 1
            let (new_figures, new_pos_info) = 
                (change_figures (Array.copy figures) (info, id), change_pos_info (Array2D.copy pos_info) figures.[id].position info)
            let cr_mas_w = (Array.copy weights_rnn, Array.copy pos_val)
            if info.fig_num > 0y then correct_mas_on_Tpawn_w (fst cr_mas_w) (snd cr_mas_w) num (int new_figures.[id].figure - 1) id
            let n1_o = neurons_1_output [| for i in 0..31 -> new_figures.[i].status |] (fst cr_mas_w).[0]
            let n2_o = neurons_2_output (snd cr_mas_w) n1_o [| for i in 0..31 -> new_figures.[i].position |]
            let n3_o = neurons_3_output n2_o (fst cr_mas_w).[1]
            let n4_o = neurons_4_output (snd cr_mas_w) new_figures new_pos_info (fst cr_mas_w) e_color n1_o n3_o num
            if n4_o.Length > 0 then
                let best_cont = 
                    let mas = [|for i in n4_o -> fst i|]
                    Array.IndexOf(mas, Array.max(mas))
                let new_n4_o = try_to_win (cur_depth + 1uy) new_figures (fst cr_mas_w) (snd cr_mas_w) n4_o.[best_cont] new_pos_info (not e_color) num
                let cor_value = if cur_depth % 2uy = 1uy then value - fst new_n4_o  else value + fst new_n4_o
                (cor_value, (info, id))
            else 
                (value, (info, id))
        else 
            (value, (info, id))
        
module Traininig = 
    open Computing
    //Статистика
    let mutable mid_error = [| 0.0; 0.0 |]
    let (stateStorage: StateStorage[]) = [| StateStorage.Default; StateStorage.Default |]
    
    //Награды, их видимо надо держать в форме [0;1], активированными по tanh
    let (b_pawn, b_light_fig, b_heavy_fig, b_queen) = (0.012, 0.036, 0.06, 0.096)//0,18 + 0,14 = 0,32
    let (b_check, b_mate, b_no_dev) = (0.032, 0.54, 0.002)//0,65 + 0,32 = 0,97
    //Рейтинг это: сумма собственных фигур - сумма фигур оппонента - состояние собственных фигур + состояние вражеских фигур
    //шах и мат > любого количества фигур противника
    
    let refresh_mut_param() = 
        mid_error <- [| 0.0; 0.0 |]
        Array.set stateStorage 0 StateStorage.Default
        Array.set stateStorage 1 StateStorage.Default
    let fill_rating (state: FigureData[]) (ai_num: int) =
        let (st_1, en_1) = if ai_num = 0 then ( 0, 15 ) else (16, 31)
        let mutable rating = 0.0
        for fig_data in state.[st_1..en_1] do
            if fig_data.status then
                if fig_data.figure <> Figure.Pawn && fig_data.state = MoveFeature.Sleep then rating <- rating - b_no_dev
                match fig_data.figure with
                | Figure.Pawn -> rating <- rating + b_pawn
                | Figure.Knight | Figure.Bishop -> rating <- rating + b_light_fig
                | Figure.Rook -> rating <- rating + b_heavy_fig
                | Figure.Queen -> rating <- rating + b_queen
                | Figure.King -> 
                    if fig_data.state = MoveFeature.Check then rating <- rating - b_check
                    if fig_data.state = MoveFeature.Mate then rating <- b_mate
                | _ -> ()
        let (st_2, en_2) = if ai_num = 0 then (16, 31) else ( 0, 15 )  
        for fig_data in state.[st_2..en_2] do
            if fig_data.status then
                if fig_data.figure <> Figure.Pawn && fig_data.state = MoveFeature.Sleep then rating <- rating + b_no_dev
                match fig_data.figure with
                | Figure.Pawn -> rating <- rating - b_pawn
                | Figure.Knight | Figure.Bishop -> rating <- rating - b_light_fig
                | Figure.Rook -> rating <- rating - b_heavy_fig
                | Figure.Queen -> rating <- rating - b_queen
                | Figure.King -> 
                    if fig_data.state = MoveFeature.Check then rating <- rating + b_check
                    if fig_data.state = MoveFeature.Mate then rating <- -b_mate
                | _ -> ()
        Math.Round(rating, 2)
    let inline add_record (state: FigureData[]) (ai_rating: float) (num: int) (n1_o: float[]) (n2_o: float[]) = 
        stateStorage.[num].states <- stateStorage.[num].states @ [state]
        let mutable pawnIden = false
        for i in 8..15 do
            if state.[i].figure <> Figure.Pawn then
                pawnIden <- true
        for i in 24..31 do
            if state.[i].figure <> Figure.Pawn then
                pawnIden <- true
        stateStorage.[num].isPawnTransform <- stateStorage.[num].isPawnTransform @ [pawnIden]
        stateStorage.[num].rating <- stateStorage.[num].rating @ [fill_rating state num]
        stateStorage.[num].ai_rating <- stateStorage.[num].ai_rating @ [ai_rating]
        stateStorage.[num].n1_o <- stateStorage.[num].n1_o @ [n1_o]
        stateStorage.[num].n2_o <- stateStorage.[num].n2_o @ [n2_o]
    
    let calculate_bvb_statistics color count ((num_1, num_2): (int * int)) = 
        let error_1 = mid_error.[0] / (if color then float (count - 1uy) else float count)
        let error_2 = mid_error.[1] / float (count - 1uy)
        let er_mas = 
            let mas = readFromJSON<float[][]> errors_path
            [| Array.append mas.[num_1] [|error_1|]; Array.append mas.[num_2] [|error_2|] |]
        writeToJson errors_path er_mas
    //Обучение
    let train () =
        [|
            for j in 0..1 do
                let counter = stateStorage.[j].isPawnTransform.Length - 1
                let pre_delta_arr = [|for i in 0..counter -> stateStorage.[j].rating.[i] - stateStorage.[j].ai_rating.[i]|]
    
                [| for i in 0..counter do 
                    mid_error.[j] <- mid_error.[j] + pre_delta_arr.[i]**2.0
                    pre_delta_arr.[i] * Computing.d_a_func_ht stateStorage.[j].ai_rating.[i] |]
        |]
    let refresh_weights (d_out_arr: float[]) (num: byte) (color: int) = 
        let w_rnn = readFromJSON<float[][][]> (weights_rnn_path num)
        let d_w_rnn = readFromJSON<float[][][]> (weights_rnn_d_path num)
        let pos_val = 
            let mas = readFromJSON<float[][]> (pos_val_path num)
            create_2DArray mas 8
        let d_pos_val = 
            let mas = readFromJSON<float[][]> (pos_val_d_path num)
            create_2DArray mas 8
        let pt_w_rnn = readFromJSON<float[][][][]> (pt_weights_rnn_path num)
        let d_pt_w_rnn = readFromJSON<float[][][][]> (pt_weights_rnn_d_path num)
        let pt_pos_val = 
            let mas = readFromJSON<float[][][]> (pt_pos_val_path num)
            [| 
                for i in 0..3 ->
                    create_2DArray mas.[i] 8
            |]
        let d_pt_pos_val = 
            let mas = readFromJSON<float[][][]> (pt_pos_val_d_path num)
            [| 
                for i in 0..3 ->
                    create_2DArray mas.[i] 8
            |]
        let counter = stateStorage.[color].isPawnTransform.Length - 1
    
        for w in 0..counter do
            let mutable (indexes: List<int>) = []
            let mutable ((s_w_rnn_0, s_w_rnn_1): (List<float[]> * List<float[]>)) = ([], [])
            let mutable ((s_d_w_rnn_0, s_d_w_rnn_1): (List<float[]> * List<float[]>)) = ([], [])
            let mutable (s_pos_val: List<float[,]>) = []
            let mutable (s_d_pos_val: List<float[,]>) = []
            let d_out = d_out_arr.[w]
            let n1_o = (List.toArray stateStorage.[color].n1_o).[w]
            let n2_o = (List.toArray stateStorage.[color].n2_o).[w]
            let figures = stateStorage.[color].states.[w]
            let f_pos = [| for i in figures -> (int (fst i.position), int (snd i.position)) |]
            let f_stat = [| for i in figures -> Convert.ToDouble(i.status)|]
            let pos_w_mas = [| for i in 0..31 -> pos_val.[i].[fst f_pos.[i], snd f_pos.[i]] |]
            
            //скорректировали веса при трансформированной пешке, и сохранили базовые веса
            if stateStorage.[color].isPawnTransform.[w] then
                for i in 0..31 do
                    if figures.[i].figure = Figure.Pawn && figures.[i].state = MoveFeature.Transform then
                        let fig_num = int figures.[i].figure - 1
                        //сохранили изменяющиеся веса
                        indexes <- indexes @ [i]
                        s_w_rnn_0 <- s_w_rnn_0 @ [w_rnn.[0].[i]]
                        s_w_rnn_1 <- s_w_rnn_1 @ [w_rnn.[1].[i]]
                        s_d_w_rnn_0 <- s_d_w_rnn_0 @ [d_w_rnn.[0].[i]]
                        s_d_w_rnn_1 <- s_d_w_rnn_1 @ [d_w_rnn.[1].[i]]
                        s_pos_val <- s_pos_val @ [pos_val.[i]]
                        s_d_pos_val <- s_d_pos_val @ [d_pos_val.[i]]
                        //изменили необходимые веса на пешечные
                        Array.set w_rnn.[0] i pt_w_rnn.[0].[fig_num].[i]
                        Array.set w_rnn.[1] i pt_w_rnn.[1].[fig_num].[i]
                        Array.set d_w_rnn.[0] i d_pt_w_rnn.[0].[fig_num].[i]
                        Array.set d_w_rnn.[1] i d_pt_w_rnn.[1].[fig_num].[i]
                        Array.set pos_val i pt_pos_val.[fig_num].[i]
                        Array.set d_pos_val i d_pt_pos_val.[fig_num].[i]
            let grad_out = 
                [| 
                    for i in n2_o -> d_out * i
                |]
            //изменение веса
            let d_w_1 = 
                [|
                    for i in 0..31 -> [| for j in 0..31 -> learning_rate * grad_out.[j] + moment * d_w_rnn.[1].[i].[j] |]
                |]
            for i in 0..31 do
                for j in 0..31 do
                    //зафиксировали изменение
                    Array.set d_w_rnn.[1].[i] j d_w_1.[i].[j]
                    //изменили вес
                    Array.set w_rnn.[1].[i] j (w_rnn.[1].[i].[j] + d_w_1.[i].[j])
    
            let delta_h_1 = 
                [|
                    for i in 0..31 -> d_a_func_ht (n2_o.[i]) * Array.sum w_rnn.[1].[i] * d_out
                |]
            let grad_h_1 = 
                [| 
                    for i in 0..31 -> n2_o.[i] * delta_h_1.[i]
                |]
            //изменение веса
            let d_w_pv = 
                [|
                    for i in 0..31 -> learning_rate * grad_h_1.[i] + moment * d_pos_val.[i].[fst f_pos.[i], snd f_pos.[i]]
                |]
            for i in 0..31 do
                //зафиксировали изменение
                Array2D.set d_pos_val.[i] (fst f_pos.[i]) (snd f_pos.[i]) d_w_pv.[i]
                //изменили вес
                Array2D.set pos_val.[i] (fst f_pos.[i]) (snd f_pos.[i]) (pos_w_mas.[i] + d_w_pv.[i])
            
            let delta_h_2 = 
                [|
                    for i in 0..31 -> d_a_func_ht (n1_o.[i]) * pos_w_mas.[i] * delta_h_1.[i]
                |]
            let grad_h_2 = 
                [| 
                    for i in 0..31 do
                        [| 
                            for j in f_stat -> delta_h_2.[i] * Convert.ToDouble(n1_o.[i]) |]
                |]
            //изменение веса
            let d_w_0 = 
                [|
                    for i in 0..31 -> [| for j in 0..31 -> learning_rate * grad_h_2.[i].[j] + moment * d_w_rnn.[0].[i].[j] |]
                |]
            for i in 0..31 do
                for j in 0..31 do   
                    //зафиксировали изменение
                    Array.set d_w_rnn.[0].[i] j d_w_0.[i].[j]
                    //изменили вес
                    Array.set w_rnn.[0].[i] j (w_rnn.[0].[i].[j] + d_w_0.[i].[j])
            //тут нужно вернуть всё на свои места
            if stateStorage.[color].isPawnTransform.[w] then
                for i in indexes do
                    let fig_num = int figures.[i].figure - 1
                    //записали посчитанные изменённые веса в пешечные веса
                    Array.set pt_w_rnn.[0].[fig_num] i w_rnn.[0].[i]
                    Array.set pt_w_rnn.[1].[fig_num] i w_rnn.[1].[i]
                    Array.set d_pt_w_rnn.[0].[fig_num] i d_w_rnn.[0].[i]
                    Array.set d_pt_w_rnn.[1].[fig_num] i d_w_rnn.[1].[i]
                    Array.set pt_pos_val.[fig_num] i pos_val.[i]
                    Array.set d_pt_pos_val.[fig_num] i d_pos_val.[i]
    
                    //вернули веса на родину
                    Array.set w_rnn.[0] i s_w_rnn_0.[i]
                    Array.set w_rnn.[1] i s_w_rnn_1.[i]
                    Array.set d_w_rnn.[0] i s_d_w_rnn_0.[i]
                    Array.set d_w_rnn.[1] i s_d_w_rnn_1.[i]
                    Array.set pos_val i s_pos_val.[i]
                    Array.set d_pos_val i s_d_pos_val.[i] 
        //записали пешечные веса            
        writeToJson (pt_weights_rnn_path num) pt_w_rnn
        writeToJson (pt_pos_val_path num) pt_pos_val
        writeToJson (pt_weights_rnn_d_path num) d_pt_w_rnn
        writeToJson (pt_pos_val_d_path num) d_pt_pos_val                
        writeToJson (weights_rnn_path num) w_rnn
        writeToJson (pos_val_path num) pos_val
        writeToJson (weights_rnn_d_path num) d_w_rnn
        writeToJson (pos_val_d_path num) d_pos_val
