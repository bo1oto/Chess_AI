using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

using ChessLib;

namespace Chess_AI_AI_UI
{
    public partial class MainWindow : Window
    {
        private const string path = "D:/VisualStudioRepos/C#/Chess_AI_AI_UI/chesscom/";
        private static bool start_color = true;
        private static Image selected_image;
        private static List<DataTypes.MoveInfo> full_moves;
        private static byte move_count = 1;
        private static Canvas cell_canvas, fig_canvas;
        public MainWindow()
        {
            InitializeComponent();
            PvsB.init_game();
            Fill_Board(false);
        }

        private void Fill_Board(bool isGame)
        {
            cell_canvas = new Canvas
            {
                Height = main_canvas.Height,
                Width = main_canvas.Width
            };
            fig_canvas = new Canvas
            {
                Height = main_canvas.Height,
                Width = main_canvas.Width
            };
            main_canvas.Children.Add(cell_canvas);
            main_canvas.Children.Add(fig_canvas);

            const short step = 80;
            short c_bot = start_color ? (short)0: (short)560;

            bool cur_color = false;
            for (byte i = 0; i < 8; i++)
            {
                short c_left = start_color ? (short)0 : (short)560;
                for (byte j = 0; j < 8; j++)
                {
                    byte id = (byte)(i * 8 + j);
                    Rectangle rectangle = new Rectangle
                    {
                        Name = "c_" + id,
                        Height = 80,
                        Width = 80,
                        Fill = cur_color ? new SolidColorBrush(Color.FromRgb(238, 238, 210)) : new SolidColorBrush(Color.FromRgb(118, 150, 85))
                    };
                    if (isGame) rectangle.MouseDown += new MouseButtonEventHandler(ToMoveClick);
                    cell_canvas.Children.Add(rectangle);
                    Canvas.SetLeft(cell_canvas.Children[id], c_left);
                    Canvas.SetBottom(cell_canvas.Children[id], c_bot);
                    cur_color = !cur_color;
                    c_left += start_color ? (short)step : (short)-step;
                }
                cur_color = !cur_color;
                c_bot += start_color ? (short)step : (short)-step;
            }
            Arrange_Figures(step, isGame);
        }
        private void Arrange_Figures(short step, bool isGame)
        {
            char color_lit = 'w';
            short c_left, c_bot;
            if (start_color) { c_left = 0; c_bot = 0; } else { c_left = 560; c_bot = 560; }
            char[] fig_lit = { 'R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R', 'P' };
            for (byte i = 0, counter = 0; i < 2; i++)
            {
                for (byte j = 0; j < 9; j++)
                {
                    BitmapImage bi3 = new BitmapImage();
                    bi3.BeginInit();
                    bi3.UriSource = new Uri(path + color_lit + fig_lit[j] + ".png", UriKind.Relative);
                    bi3.CacheOption = BitmapCacheOption.OnLoad;
                    bi3.EndInit();

                    byte z = 1;
                    if (j == 8)
                    {
                        z = 8;
                        if (i == 0 && start_color || i == 1 && !start_color) c_bot += step; else c_bot -= step;
                        if (start_color) c_left = 0;  else c_left = 560;
                    }
                    for (; z > 0; z--)
                    {
                        Image image = new Image
                        {
                            Stretch = Stretch.None,
                            Source = bi3,
                            Name = "f_" + Convert.ToString(counter)
                        };
                        if (isGame && ((i == 0 && start_color) || i == 1 && !start_color))
                            image.MouseDown += new MouseButtonEventHandler(OnFigureClick);
                        else 
                            image.IsHitTestVisible = false;
                        fig_canvas.Children.Add(image);
                        Canvas.SetBottom(fig_canvas.Children[counter], c_bot);
                        Canvas.SetLeft(fig_canvas.Children[counter], c_left);
                        c_left += start_color ? step : (short)-step;
                        counter++;
                    }
                }
                if (start_color) { c_left = 0; c_bot = 560; } else { c_left = 560; c_bot = 0; }
                color_lit = 'b';
            }
        }

        private void OnFigureClick(object sender, MouseButtonEventArgs e)
        {
            float circle_to_center = 27.5f;
            selected_image = (Image)sender;
            text.Text = selected_image.Name;

            for (byte i = 0, count = (byte)(main_canvas.Children.Count - 2); i < count; i++)
            {
                main_canvas.Children.Remove(main_canvas.Children[2]);
            }
            int num = Convert.ToInt32(selected_image.Name.Substring(2));
            full_moves = PvsB.get_fig_moves(!start_color, num).ToList();

            for (byte i = 0, count = (byte)full_moves.Count; i < count; i++)
            {
                Ellipse ellipse = new Ellipse
                {
                    Height = 25,
                    Width = 25,
                    Fill = new SolidColorBrush(Color.FromRgb(0, 0, 0)),
                    IsHitTestVisible = false,
                    Opacity = 0.15
                };
                main_canvas.Children.Add(ellipse);
                int cell_id = full_moves[i].move.Item1 + full_moves[i].move.Item2 * 8;
                Canvas.SetLeft(main_canvas.Children[2 + i], Canvas.GetLeft(cell_canvas.Children[cell_id]) + circle_to_center);
                Canvas.SetBottom(main_canvas.Children[2 + i], Canvas.GetBottom(cell_canvas.Children[cell_id]) + circle_to_center);
            }
        }
        private async void ToMoveClick(object sender, MouseButtonEventArgs e)
        {
            for (byte i = 0, count = (byte)(main_canvas.Children.Count - 2); i < count; i++)
            {
                main_canvas.Children.Remove(main_canvas.Children[2]);
            }
            byte cell_id = Convert.ToByte(((FrameworkElement)sender).Name.Substring(2));
            int move_id = -1;
            for (int i = 0, count = full_moves.Count; i < count; i++)
            {
                if (full_moves[i].move.Item1 + full_moves[i].move.Item2 * 8 == cell_id)
                {
                    move_id = i;
                    break;
                }
            }
            if (move_id != -1)
            {
                //If there is an attack
                if (full_moves[move_id].id != -1)
                {
                    fig_canvas.Children[full_moves[move_id].id].Visibility = Visibility.Hidden;
                }
                if (full_moves[move_id].fig_num != -1)
                {
                    //Castling
                    if (full_moves[move_id].fig_num == 0)
                    {
                        (int r1, int r2, int cell) = start_color ? (0, 7, 0) : (16, 23, 7);
                        if (full_moves[move_id].move.Item1 > 4)
                        {
                            Canvas.SetLeft(fig_canvas.Children[r2], Canvas.GetLeft(cell_canvas.Children[5]));
                            Canvas.SetBottom(fig_canvas.Children[r2], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                        }
                        else
                        {
                            Canvas.SetLeft(fig_canvas.Children[r1], Canvas.GetLeft(cell_canvas.Children[3]));
                            Canvas.SetBottom(fig_canvas.Children[r1], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                        }

                    }
                    //Pawn transformation
                    else
                    {
                        main_canvas.IsHitTestVisible = false;
                        Image[] images = { t_q, t_n, t_r, t_b };
                        for (byte i = 0; i < 4; i++) images[i].Visibility = Visibility.Visible;
                    }
                }
                Canvas.SetLeft(selected_image, Canvas.GetLeft(cell_canvas.Children[cell_id]));
                Canvas.SetBottom(selected_image, Canvas.GetBottom(cell_canvas.Children[cell_id]));
                await Task.Delay(100);
                    
                if (full_moves[move_id].fig_num == -1 || full_moves[move_id].fig_num == 0)
                {
                    PassMove(move_id);
                    move_count = start_color ? move_count : (byte)(move_count + 1);
                    count_info.Text = "Ход: " + move_count;
                    full_moves.Clear();
                }

            }
            else full_moves.Clear();
        }
        private void TransformClick(object sender, MouseButtonEventArgs e)
        {
            text.Text = "На месте";
            main_canvas.IsHitTestVisible = true;
            byte cell_left_id = (byte)(Canvas.GetLeft(selected_image) / 80);
            if (!start_color)
                cell_left_id = (byte)(7 - cell_left_id);
            selected_image.Source = ((Image)sender).Source;
            sbyte num;
            switch (((FrameworkElement)sender).Name)//q, r, b, n
            {
                case "t_q": num = 1; break;
                case "t_r": num = 2; break;
                case "t_b": num = 3; break;
                case "t_n": num = 4; break;
                default: num = -1; break;
            }
            int move_id = -1; 
            for (byte i = 0, count = (byte)full_moves.Count; i < count; i++)
            {
                if (full_moves[i].fig_num == num && full_moves[i].move.Item1 == cell_left_id)
                {
                    move_id = i;
                    break;
                }
            }
            Image[] images = { t_q, t_n, t_r, t_b };
            for (byte i = 0; i < 4; i++) images[i].Visibility = Visibility.Hidden;

            PassMove(move_id);
            full_moves.Clear();
        }

        private void PassMove(int move_id)
        {
            //Transferred their turn and received the bot's turn
            Tuple<byte, byte, Tuple<DataTypes.MoveInfo, int>> result; 
            if (move_id != -1)
            {
                int ids = Convert.ToInt32(selected_image.Name.Substring(2));
                result = PvsB.pvb_game(full_moves[move_id], ids, !start_color, move_count);
            }
            else
            {
                result = PvsB.pvb_game(DataTypes.MoveInfo.Default, -1, !start_color, move_count);
            }
            move_count = result.Item1;
            if (result.Item2 != 0)
            {
                PvsB.calculate_pvb_statistics(!start_color, move_count); 
                if (result.Item2 == 1)
                {
                    string name = result.Item3.Item2 == 32 ? "AI_1 win!" : "You win!";
                    text.Text = name;
                }
                else
                {
                    text.Text = "Combat draw!";
                }
            }
            selected_image = (Image)fig_canvas.Children[result.Item3.Item2];
            byte cell_id = (byte)(result.Item3.Item1.move.Item1 + result.Item3.Item1.move.Item2 * 8);
            if (result.Item3.Item1.id != -1)
            {
                ((Image)fig_canvas.Children[result.Item3.Item1.id]).Visibility = Visibility.Hidden;
            }
            if (result.Item3.Item1.fig_num != -1)
            {
                //Castling
                if (result.Item3.Item1.fig_num == 0)
                {
                    (int r1, int r2, int cell) = start_color ? (16, 23, 7) : (0, 7, 0);
                    if (result.Item3.Item1.move.Item1 > 4)
                    {
                        Canvas.SetLeft(fig_canvas.Children[r2], Canvas.GetLeft(cell_canvas.Children[5]));
                        Canvas.SetBottom(fig_canvas.Children[r2], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                    }
                    else
                    {
                        Canvas.SetLeft(fig_canvas.Children[r1], Canvas.GetLeft(cell_canvas.Children[3]));
                        Canvas.SetBottom(fig_canvas.Children[r1], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                    }

                }
                //Pawn transformation
                else
                {
                    char[] fig_lit = { 'Q', 'N', 'R', 'B' };
                    char color_lit = start_color ? 'b' : 'w';
                    BitmapImage bi3 = new BitmapImage();
                    bi3.BeginInit();
                    bi3.UriSource = new Uri(path + color_lit + fig_lit[result.Item3.Item1.fig_num - 1] + ".png", UriKind.Relative);
                    bi3.CacheOption = BitmapCacheOption.OnLoad;
                    bi3.EndInit();
                    ((Image)fig_canvas.Children[result.Item3.Item2]).Source = bi3;
                }
            }
            Canvas.SetLeft(selected_image, Canvas.GetLeft(cell_canvas.Children[cell_id]));
            Canvas.SetBottom(selected_image, Canvas.GetBottom(cell_canvas.Children[cell_id]));
        }
        
        private async void Watch_bvb(object sender, RoutedEventArgs e)
        {
            byte num_1 = 1, num_2 = 2;
            w_play.IsEnabled = false;
            b_play.IsEnabled = false;
            reset_but.IsEnabled = true;
            pause_but.IsEnabled = true;


            while (!isPause)
            {
                Tuple<byte, byte, Tuple<DataTypes.MoveInfo, int>> result = BvsB.bvb_game(DataTypes.PartyState.Play, start_color, move_count, num_1, num_2);
                selected_image = (Image)fig_canvas.Children[result.Item3.Item2];
                byte cell_id = (byte)(result.Item3.Item1.move.Item1 + result.Item3.Item1.move.Item2 * 8);
                if (result.Item3.Item1.id != -1)
                {
                    ((Image)fig_canvas.Children[result.Item3.Item1.id]).Visibility = Visibility.Hidden;
                }
                if (result.Item3.Item1.fig_num != -1)
                {
                    //Castling
                    if (result.Item3.Item1.fig_num == 0)
                    {
                        (int r1, int r2, int cell) = start_color ? (16, 23, 7) : (0, 7, 0);
                        if (result.Item3.Item1.move.Item1 > 4)
                        {
                            Canvas.SetLeft(fig_canvas.Children[r2], Canvas.GetLeft(cell_canvas.Children[5]));
                            Canvas.SetBottom(fig_canvas.Children[r2], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                        }
                        else
                        {
                            Canvas.SetLeft(fig_canvas.Children[r1], Canvas.GetLeft(cell_canvas.Children[3]));
                            Canvas.SetBottom(fig_canvas.Children[r1], Canvas.GetBottom(cell_canvas.Children[cell * 8]));
                        }

                    }
                    //Pawn transformation
                    else
                    {
                        char[] fig_lit = { 'Q', 'N', 'R', 'B' };
                        char color_lit = start_color ? 'b' : 'w';
                        BitmapImage bi3 = new BitmapImage();
                        bi3.BeginInit();
                        bi3.UriSource = new Uri(path + color_lit + fig_lit[result.Item3.Item1.fig_num - 1] + ".png", UriKind.Relative);
                        bi3.CacheOption = BitmapCacheOption.OnLoad;
                        bi3.EndInit();
                        ((Image)fig_canvas.Children[result.Item3.Item2]).Source = bi3;
                    }
                }
                Canvas.SetLeft(selected_image, Canvas.GetLeft(cell_canvas.Children[cell_id]));
                Canvas.SetBottom(selected_image, Canvas.GetBottom(cell_canvas.Children[cell_id]));

                start_color = !start_color;
                move_count = result.Item1;
                count_info.Text = "Turn: " + move_count;
                switch (result.Item2)
                {
                    case 1:
                        char name = start_color ? Convert.ToChar(num_2) : Convert.ToChar(num_1);
                        text.Text = $"Победил AI_{name}!";
                        goto end;
                    case 2:
                        text.Text = "Боевая ничья!";
                        goto end;
                }
                await Task.Delay(100);
            }
            return;
        end:;
            w_play.IsEnabled = true;
            b_play.IsEnabled = true;
            pause_but.IsEnabled = false;
        }

        private void Choose_color(object sender, RoutedEventArgs e)
        {
            switch (((FrameworkElement)sender).Name[0])
            {
                case 'b': start_color = false; break;
                case 'w': start_color = true; break;
            }
            w_play.IsEnabled = false;
            b_play.IsEnabled = false;
            reset_but.IsEnabled = true;
            bvb_but.IsEnabled = false;
            Fill_Board(true);
            main_canvas.Children.Remove(main_canvas.Children[1]); main_canvas.Children.Remove(main_canvas.Children[0]);
            
            char[] fig_lit = { 'Q', 'R', 'N', 'B' };
            char color_lit = start_color ? 'w' : 'b';
            Image[] images = { t_q, t_n, t_r, t_b };
            for (byte i = 0; i < 4; i++)
            {
                BitmapImage bi3 = new BitmapImage();
                bi3.BeginInit();
                bi3.UriSource = new Uri(path + color_lit + fig_lit[i] + ".png", UriKind.Relative);
                bi3.CacheOption = BitmapCacheOption.OnLoad;
                bi3.EndInit();
                images[i].Source = bi3;
            }
            if (!start_color) PassMove(-1);

        }

        private static bool isPause = false;
        private void PauseClick(object sender, RoutedEventArgs e)
        {
            isPause = !isPause;
            if (isPause)
            {
                text.Text = "Pause";
                pause_but.Content = "Unpause";
                pause_but.Click += Watch_bvb;
            }
            else
            {
                text.Text = "Unpause";
                pause_but.Content = "Pause";
                pause_but.Click -= Watch_bvb;
            }
        }

        private void Reset(object sender, RoutedEventArgs e)
        {
            w_play.IsEnabled = true;
            b_play.IsEnabled = true;
            reset_but.IsEnabled = false;
            bvb_but.IsEnabled = true;
            move_count = 1;
            count_info.Text = "Ход: 1";
            main_canvas.Children.Remove(main_canvas.Children[1]); main_canvas.Children.Remove(main_canvas.Children[0]);

            PvsB.init_game();
            Fill_Board(false);
        }
    }
}
