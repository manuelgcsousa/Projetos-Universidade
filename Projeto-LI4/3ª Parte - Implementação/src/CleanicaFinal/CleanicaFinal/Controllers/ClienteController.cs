using CleanicaFinal.Models;
using System;
using System.Linq;
using System.Net.Mail;
using System.Text.RegularExpressions;
using System.Web.Mvc;

namespace CleanicaFinal.Controllers
{
    public class ClienteController : Controller
    {
        private String email = "";
        private String username = "";
        private CleanicaContext db = new CleanicaContext();
        private FuncServContext fs = new FuncServContext();

        /* Página Principal Cliente */
        public ActionResult Index(String email, String username, String info)
        {
            this.email = email;
            this.username = username;
            ViewBag.username = username;
            ViewBag.email = email;
            ViewBag.info = info;

            var servicos = from t in db.ServicosRealizados
                           where t.Cliente_Utilizador_email == email
                           select t;


            return View(servicos);
        }

        /* Página Serviços Disponíveis */
        public ActionResult GetServicos(String email, String username)
        {
            ViewBag.username = username;
            ViewBag.email = email;

            var servicos = from t in db.Servicoes
                           select t;

            return View(servicos);
        }

        /* Página Formulários Pendentes */
        public ActionResult TarefasPendentes(String email, String username)
        {
            ViewBag.username = username;
            ViewBag.email = email;

            var formulariosPendentes = from t in db.Formulario_de_Satisfacao
                                       where t.Cliente_Utilizador_email == email && t.pendente == "S"
                                       select t;

            return View(formulariosPendentes);
        }

        /* Página Mostra Formulário Para Preencher */
        public ActionResult MostrarForm(String email, String username, String idForm)
        {
            ViewBag.username = username;
            ViewBag.email = email;
            ViewBag.idForm = idForm;

            int x = Int32.Parse(idForm);

            var formParaPreencher = from t in db.Formulario_de_Satisfacao
                                    where t.id_FormularioS == x
                                    select t;

            return View(formParaPreencher);
        }

        /* Submete o Formulário de Satisfação Preenchido */
        public ActionResult PreencheFormSatisfacao(String email, String username, String idForm, String pont, String sugs)
        {
            ViewBag.username = username;
            ViewBag.email = email;
            String info = "";
            int idF = 0;
            int p = 0;

            try
            {
                idF = Int32.Parse(idForm);
                p = Int32.Parse(pont);
                if (!(p > 0 && p <= 5))
                {
                    info = "Introduza Um Valor Entre 1 e 5 No Campo Pontuação";
                    return RedirectToAction("Index", "Cliente", new { email, username, info });
                }

                var query = from t in db.Formulario_de_Satisfacao
                            where t.id_FormularioS == idF
                            select t;

                foreach (Formulario_de_Satisfacao f in query)
                {
                    /* Update na Base de Dados */
                    f.pontuacao = p;
                    f.sugestoes = sugs;
                    f.pendente = "F";
                }

                db.SaveChanges();
                info = "Formulário Submetido Com Sucesso";
                return RedirectToAction("Index", "Cliente", new { email, username, info });

            } catch (Exception) {
                info = "Não Foi Possível Submeter Formulário.\nPor Favor Tente Mais Tarde";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }
        }

        /* Página Alterar Dados Cliente */
        public ActionResult AlterarDados(String email, String username)
        {
            ViewBag.username = username;
            ViewBag.email = email;

            var viewModel = from u in db.Clientes
                            where u.Utilizador_email == email
                            select u;

            var utilizador = from u in db.Utilizadors
                             where u.email == email
                             select u;

            foreach (var item in utilizador)
            {
                ViewBag.username = item.username;
                ViewBag.password = item.password;
            }

            return View(viewModel);
        }

        /* Submeter Alteração de Dados */
        public ActionResult AlteraDadosCliente(String email, String username, String password, String morada, String telemovel)
        {
            ViewBag.username = username;
            ViewBag.email = email;
            String info = "";

            Regex tlmv = new Regex("9[1236][0-9]{7}$");

            if (!tlmv.IsMatch(telemovel))
            {
                info = "Formato Inválido Para o Campo Telemóvel";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }

            try
            {
                var query = from t in db.Clientes
                            where t.Utilizador_email == email
                            select t;

                foreach (Cliente f in query)
                {
                    // Update na Base de Dados // 
                    f.morada = morada;
                    f.telemovel = Int32.Parse(telemovel);
                }

                var query2 = from t in db.Utilizadors
                             where t.email == email
                             select t;

                foreach (Utilizador f in query2)
                {
                    // Update na Base de Dados // 
                    f.username = username;
                    f.password = password;
                }

                db.SaveChanges();

            } catch (Exception) {
                info = "Não Foi Possível Atualizar Os Dados";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }

            info = "Dados Alterados Com Sucesso";
            return RedirectToAction("Index", "Cliente", new { email, username, info });
        }

        /* Página Formulário Requisitar Serviço Jardinagem */
        public ActionResult JardinagemForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            // Calcular Datas 
            String[] dates = GetDates();

            ViewBag.min = dates[0];
            ViewBag.max = dates[1];

            var query = (from t in db.Servicoes
                         where t.id_servico == 1
                         select t).FirstOrDefault();

            ViewBag.precoCProds = query.preco.ToString().Replace(",", ".");
            ViewBag.precoSProds = query.precoSProduto.ToString().Replace(",",".");

            return View();
        }

        /* Página Formulário Requisitar Serviço Exteriores */
        public ActionResult ExterioresForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            // Calcular Datas 
            String[] dates = GetDates();

            ViewBag.min = dates[0];
            ViewBag.max = dates[1];

            var query = (from t in db.Servicoes
                         where t.id_servico == 2
                         select t).FirstOrDefault();

            ViewBag.precoCProds = query.preco.ToString().Replace(",", ".");
            ViewBag.precoSProds = query.precoSProduto.ToString().Replace(",", ".");

            return View();
        }

        /* Página Formulário Requisitar Serviço Carros */
        public ActionResult CarrosForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            // Calcular Datas 
            String[] dates = GetDates();

            ViewBag.min = dates[0];
            ViewBag.max = dates[1];

            var query = (from t in db.Servicoes
                         where t.id_servico == 3
                         select t).FirstOrDefault();

            ViewBag.precoCProds = query.preco.ToString().Replace(",", ".");
            ViewBag.precoSProds = query.precoSProduto.ToString().Replace(",", ".");

            return View();
        }

        /* Página Formulário Requisitar Serviço Interiores */
        public ActionResult InterioresForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            // Calcular Datas 
            String[] dates = GetDates();

            ViewBag.min = dates[0];
            ViewBag.max = dates[1];

            var query = (from t in db.Servicoes
                         where t.id_servico == 4
                         select t).FirstOrDefault();

            ViewBag.precoCProds = query.preco.ToString().Replace(",", ".");
            ViewBag.precoSProds = query.precoSProduto.ToString().Replace(",", ".");

            return View();
        }

        public ActionResult RequisitaServico(String email, String username, String morada, int idServico,
            String dimensao, String produtos, String consideracoes, DateTime date, String nif)
        {
            String info = ""; 
            Regex r = new Regex("^[0-9]{9}$");

            ViewBag.email = email;
            ViewBag.username = username;

            if (!r.IsMatch(nif))
            {
                info = "NIF Incorreto";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }

            int dim = 0;
            String[] func = new string[3];

            /* Funcionários Para o Serviço */
            try
            {
                if (dimensao == "P")
                {
                    dim = 1;
                    func = Get1Func(idServico, date);
                }
                if (dimensao == "M")
                {
                    dim = 2;
                    func = Get2Func(idServico, date);
                }
                if (dimensao == "G")
                {
                    dim = 3;
                    func = Get3Func(idServico, date);
                }
            } catch (Exception)
            {
                info = "Não existem funcionários disponíveis nessa data. Lamentamos a inconveniência";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }
            
            /* Pagamento */ 
            float preco = CalculaPreco(idServico, dim, produtos);
            
            /* Adicionar Novos Serviços a Realizar na Base de Dados */
            try
            {
                ServicosRealizado sR = new ServicosRealizado()
                {
                    Cliente_Utilizador_email = email,
                    morada = morada,
                    data = date,
                    preco = preco,
                    dimensao = dimensao,
                    prods = produtos,
                    aspectosimp = consideracoes,
                    pendente = "S",
                    duracao = dim
                };

                db.ServicosRealizados.Add(sR);
                db.SaveChanges();

                int count = 0;

                var x = (from t in db.ServicosRealizados
                         where t.Cliente_Utilizador_email == email && t.data == date
                         select t);

                foreach (var item in x)
                {
                    count = item.id_ServicosRealizados;
                }

                for (int i = 0; i < dim; i++)
                {
                    Funcionario_ServicosRealizados fSR = new Funcionario_ServicosRealizados()
                    {
                        Funcionario_Utilizador_email = func[i],
                        ServicosRealizados_id_ServicosRealizados = count
                    };

                    fs.Funcionario_ServicosRealizados.Add(fSR);
                }

                fs.SaveChanges();

            } catch (Exception)
            {
                info = "Ocorreu um erro. Lamentamos";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }

            try
            {
                /* Gerar e Enviar Fatura para o Email */
                GeraFatura(email, nif, date);
            } catch (Exception)
            {
                info = "Não foi possível enviar email de confirmação. No entanto o seu pedido ficou registado. Obrigado";
                return RedirectToAction("Index", "Cliente", new { email, username, info });
            }
            
            info = "O Serviço Foi Requisitado Com Sucesso. Foi enviada a fatura para o seu email.";

            return RedirectToAction("Index","Cliente", new { email, username, info });
        }

        /*******************        AUXILIARES      *******************/

        /* Retorna as Datas Min e Max para Requisitar Serviços (1 Semana) */
        public String[] GetDates()
        {
            TimeSpan t = new TimeSpan(7, 0, 0, 0);
            TimeSpan w = new TimeSpan(1, 0, 0, 0);
            DateTime d = DateTime.Now;
            DateTime dB = d.Add(w);
            DateTime dA = d.Add(t);

            String year = dB.Year.ToString();
            String month = dB.Month.ToString();
            String day = dB.Day.ToString();
            String hour = dB.Hour.ToString();
            String minute = dB.Minute.ToString();

            String yearA = dA.Year.ToString();
            String monthA = dA.Month.ToString();
            String dayA = dA.Day.ToString();
            String hourA = dA.Hour.ToString();
            String minuteA = dA.Minute.ToString();

            Regex m = new Regex("[0-9]{2}");

            if (!m.IsMatch(month)) month = "0" + month;
            if (!m.IsMatch(day)) day = "0" + day;
            if (!m.IsMatch(hour)) hour = "0" + hour;
            if (!m.IsMatch(minute)) minute = "0" + minute;

            if (!m.IsMatch(monthA)) monthA = "0" + monthA;
            if (!m.IsMatch(dayA)) dayA = "0" + dayA;
            if (!m.IsMatch(hourA)) hourA = "0" + hourA;
            if (!m.IsMatch(minuteA)) minuteA = "0" + minuteA;


            String min = year + "-" + month + "-" + day + "T" + hour + ":" + minute;
            String max = yearA + "-" + monthA + "-" + dayA + "T" + hourA + ":" + minuteA;

            String[] dates = { min, max };

            return dates;
        }

        public String[] Get1Func (int idServico, DateTime date)
        {
            int size = 0;
            String[] func = new string[1];

            /* Recolher todos os Funcionários de um certo Serviço */
        var funcs = from t in db.Funcionarios
                        where t.Servico_id_servico == idServico
                        select t;
            
            /* Verificar se a hora escolhida já n está disponível */
            foreach (var x in funcs)
            {
                int count = 0;

                var servs = from t in fs.Funcionario_ServicosRealizados
                            where t.Funcionario_Utilizador_email == x.Utilizador_email
                            select t;

                foreach (var item in servs)
                {
                    int c = (from t in db.ServicosRealizados
                             where t.id_ServicosRealizados == item.ServicosRealizados_id_ServicosRealizados && t.data == date
                             select t).Count();

                    count += c;
                }
                /* Se count == 0 então a hora está livre naquele funcionário */
                if (count == 0)
                {
                    func[size] = x.Utilizador_email;
                    size++;
                }
                if (size == 1) break;
            }

            if (size == 1) return func;
            else throw new Exception();
        }

        public String[] Get2Func(int idServico, DateTime date)
        {
            int size = 0;
            String[] func = new string[2];

            /* Recolher todos os Funcionários de um certo Serviço */
            var funcs = from t in db.Funcionarios
                        where t.Servico_id_servico == idServico
                        select t;

            /* Verificar se a hora escolhida já n está disponível */
            foreach (var x in funcs)
            {
                int count = 0;

                var servs = from t in fs.Funcionario_ServicosRealizados
                            where t.Funcionario_Utilizador_email == x.Utilizador_email
                            select t;

                foreach (var item in servs)
                {
                    int c = (from t in db.ServicosRealizados
                             where t.id_ServicosRealizados == item.ServicosRealizados_id_ServicosRealizados && t.data == date
                             select t).Count();

                    count += c;
                }
                /* Se count == 0 então a hora está livre naquele funcionário */
                if (count == 0)
                {
                    func[size] = x.Utilizador_email;
                    size++;
                }
                if (size == 2) break;
            }

            if (size == 2) return func;
            else throw new Exception();
        }

        public String[] Get3Func(int idServico, DateTime date)
        {
            int size = 0;
            String[] func = new string[3];

            /* Recolher todos os Funcionários de um certo Serviço */
            var funcs = from t in db.Funcionarios
                        where t.Servico_id_servico == idServico
                        select t;

            /* Verificar se a hora escolhida já n está disponível */
            foreach (var x in funcs)
            {
                int count = 0;

                var servs = from t in fs.Funcionario_ServicosRealizados
                            where t.Funcionario_Utilizador_email == x.Utilizador_email
                            select t;

                foreach (var item in servs)
                {
                    int c = (from t in db.ServicosRealizados
                             where t.id_ServicosRealizados == item.ServicosRealizados_id_ServicosRealizados && t.data == date
                             select t).Count();

                    count += c;
                }
                /* Se count == 0 então a hora está livre naquele funcionário */
                if (count == 0)
                {
                    func[size] = x.Utilizador_email;
                    size++;
                }
                if (size == 3) break;
            }
            if (size == 3) return func;
            else throw new Exception();
        }

        private float CalculaPreco(int idServico, int dim, string produtos)
        {
            float preco = 0;

            if (produtos == "S")
            {
                var query = (from t in db.Servicoes
                             where t.id_servico == idServico
                             select t).FirstOrDefault();

                preco = dim * query.preco;
            }

            if (produtos == "N")
            {
                var query = (from t in db.Servicoes
                             where t.id_servico == idServico
                             select t).FirstOrDefault();

                preco = dim * query.precoSProduto;
            }

            return preco;
        }

        
        private void GeraFatura(string email, string nif, DateTime date)
        {
            string your_id = "cleanica@outlook.com";
            string your_password = "LIlilili";
            string body = "Ficou registado o seu pedido para " + date + ".\nNIF = " + nif + ".";

            try
            {
                SmtpClient client = new SmtpClient
                {
                    Host = "smtp.live.com",
                    Port = 587,
                    EnableSsl = true,
                    DeliveryMethod = SmtpDeliveryMethod.Network,
                    Credentials = new System.Net.NetworkCredential(your_id, your_password),
                    Timeout = 10000,
                };
                MailMessage mm = new MailMessage(your_id, email, "Cleanica - Confirmação de Serviço", body);
                client.Send(mm);
            }
            catch (Exception) { }
        }
    }
}