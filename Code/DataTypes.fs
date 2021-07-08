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
| Sleep = 4uy//The figure has not walked yet
| Check = 5uy
| Mate = 6uy

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
        color : bool //true - white, false - black
    }     

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
        id: sbyte// -1 = does not attack,
        fig_num: sbyte //1, 2, 3, 4 = figure, -1 = no transformation, 0 = castling
    }
    static member Default = 
        {
            move = (0, 0)
            id = -1y
            fig_num = -1y
        }

module Variables =
    //training parameters
    let epoch = 100
    let learning_rate = 0.0001
    let moment = 0.2
    let discount_factor = 0.8
    
    let best_move_count = 10
    let depth = 40uy//40 moves with figures, i.e. 20 moves in total
    let eps_count = 70.0
    let mutable epsilon = 0.0
    
    //Rewards, they apparently need to be kept in the form [0; 1], activated by tanh
    let (b_pawn, b_light_fig, b_heavy_fig, b_queen) = (0.012, 0.036, 0.06, 0.096)
    let (b_check, b_mate, b_no_dev) = (0.032, 0.54, 0.002)
    
    //Statistics
    let mutable mid_error = [| 0.0; 0.0 |]
    let (stateStorage: StateStorage[]) = [| StateStorage.Default; StateStorage.Default |]
    
module VariablesForIChess =
    //IChess_Lib use variables from here
    let mutable (weights_rnn_pvb : float[][][]) = [||]
    let mutable (pos_val_pvb: float[,][]) = [||]
    let bot_num = 1uy
    let mutable ((b_move_info_pvb, b_move_id_pvb): MoveInfo * int) = (MoveInfo.Default, 0)
    let mutable mid_error_pvb = 0.0
