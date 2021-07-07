namespace DataTypes


module WeightsPaths =
    let pos_path = "D:/Fast_weights//position.json"
    let move_log_path = "D:/Fast_weights//move_log.json"

    let errors_path = "D:/Fast_weights//errors.json"
    let pvb_errors_path = "D:/Fast_weights//pvb_stat/errors.json"

    let pos_val_path (num: byte) = $"D:/Fast_weights//AI_{num}/pos_val.json"
    let weights_rnn_path (num: byte) = $"D:/Fast_weights//AI_{num}/weights_rnn.json"
    let pt_weights_rnn_path (num: byte) = $"D:/Fast_weights//AI_{num}/pt_weights_rnn.json"
    let pt_pos_val_path (num: byte) = $"D:/Fast_weights//AI_{num}/pt_pos_val.json"

    let pos_val_d_path (num: byte) = $"D:/Fast_weights//AI_{num}/pos_val_d.json"
    let weights_rnn_d_path (num: byte) = $"D:/Fast_weights//AI_{num}/weights_rnn_d.json"
    let pt_weights_rnn_d_path (num: byte) = $"D:/Fast_weights//AI_{num}/pt_weights_rnn_d.json"
    let pt_pos_val_d_path (num: byte) = $"D:/Fast_weights//AI_{num}/pt_pos_val_d.json"

   
type Figure =
| King = 0uy
| Queen = 1uy
| Rook = 2uy
| Bishop = 3uy
| Knight = 4uy 
| Pawn = 5uy 
| Empty = 6uy
type PartyState = 
| Play = 0uy
| Dead = 1uy
| Draw = 2uy
type MoveFeature =
| Simple = 0uy
| ForwardMove = 1uy//Pawn
| Castling = 2uy//King, Rook
| Transform = 3uy//Pawn
| Sleep = 4uy//Фигура ещё не ходила
| Check = 5uy
| Mate = 6uy

//Данные
type FigureData = 
    {
        position: (int * int)
        figure: Figure
        status: bool
        state: MoveFeature
    }
    static member Default = 
        {
            position = (0, 0)
            figure = Figure.Pawn
            status = true
            state = MoveFeature.Sleep
        }
type PositionInfo = 
    {
        figure : Figure
        color : bool //true - белый, false - чёрный
    }     
//Контейнер состояний
type StateStorage = 
    {
        mutable states: List<FigureData[]>
        mutable isPawnTransform: List<bool>
        mutable rating: List<float> 
        mutable ai_rating: List<float> 
        mutable n1_o: List<float[]> 
        mutable n2_o: List<float[]> 
    }
    static member Default =
        {
            states = []
            isPawnTransform = []
            rating = []
            ai_rating = []
            n1_o = []
            n2_o = []
        }        
type MoveInfo = 
    {
        move: (int * int)
        id: sbyte// -1 = не атакует,
        fig_num: sbyte //1, 2, 3, 4 = соответствующие фигуры, -1 = нет трансформации, 0 = рокировка
    }
    static member Default = 
        {
            move = (0, 0)
            id = -1y
            fig_num = -1y
        }

module Variables =
    //Параметры вручную
    let epoch = 100
    let learning_rate = 0.0001
    let moment = 0.2
    let discount_factor = 0.8
    //Константы
    let best_move_count = 10
    let depth = 40uy//40 ходов фигурами, т.е. 20 ходов всего
    let eps_count = 70.0
    //Награды, их видимо надо держать в форме [0;1], активированными по tanh
    let (b_pawn, b_light_fig, b_heavy_fig, b_queen) = (0.012, 0.036, 0.06, 0.096)//0,18 + 0,14 = 0,32
    let (b_check, b_mate, b_no_dev) = (0.032, 0.54, 0.002)//0,65 + 0,32 = 0,97
    //Рейтинг это: сумма собственных фигур - сумма фигур оппонента - состояние собственных фигур + состояние вражеских фигур
    //шах и мат > любого количества фигур противника
    //Эпсилон
    let mutable epsilon = 0.0
    //Статистика
    let mutable mid_error = [| 0.0; 0.0 |]
    let (stateStorage: StateStorage[]) = [| StateStorage.Default; StateStorage.Default |]
module VariablesForIChess =
    let mutable (weights_rnn_pvb : float[][][]) = [||]
    let mutable (pos_val_pvb: float[,][]) = [||]
    let bot_num = 1uy
    let mutable ((b_move_info_pvb, b_move_id_pvb): MoveInfo * int) = (MoveInfo.Default, 0)
    let mutable mid_error_pvb = 0.0