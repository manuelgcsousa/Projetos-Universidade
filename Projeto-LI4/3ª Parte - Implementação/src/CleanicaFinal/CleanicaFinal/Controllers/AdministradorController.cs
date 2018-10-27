using System;
using System.Collections.Generic;
using System.Data.Entity.Validation;
using System.Linq;
using System.Text.RegularExpressions;
using System.Web;
using System.Web.Mvc;

namespace CleanicaFinal.Controllers
{
    public class AdministradorController : Controller
    {
        CleanicaContext db = new CleanicaContext();

        /* Página Principal Administrador */
        public ActionResult Index(String email, String username, String info)
        {
            ViewBag.info = info;
            ViewBag.email = email;
            ViewBag.username = username;

            return View();
        }

        /* Página Registar Funcionário Administrador */
        public ActionResult RegistarFuncForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            var servs = from t in db.Servicoes
                        select t;

            return View(servs);
        }

        /* Método que adiciona um novo funcionário */
        public ActionResult addFuncionario(String email, String username, String emailF, String usernameF,
            String passwordF, int servico, String cc, String telemovel, String IBAN, String dataNasc, String morada)
        {
            String info = "";

            Regex mail = new Regex(".*@(hotmail|gmail|outlook).com");
            Regex c = new Regex("[0-9]+");
            Regex number = new Regex("9[1236][0-9]{7}");
            Regex nib = new Regex("PT50[0-9]{21}");
            Regex dt = new Regex("[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}");

            if (!mail.IsMatch(emailF))
            {
                info = "Email Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }
            
            if (!c.IsMatch(cc))
            {
                info = "Cartão de Cidadão Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            if (!number.IsMatch(telemovel))
            {
                info = "Número de Telemóvel Inválido Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            if (!nib.IsMatch(IBAN))
            {
                info = "IBAN Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            if (!dt.IsMatch(dataNasc))
            {
                info = "Data de Nascimento Inválida";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }


            Utilizador u = new Utilizador
            {
                email = emailF,
                username = usernameF,
                password = passwordF,
                estatuto = "F"
            };

            Funcionario f = new Funcionario
            {
                Utilizador_email = emailF,
                Servico_id_servico = servico,
                cc = Int32.Parse(cc),
                telemovel = Int32.Parse(telemovel),
                IBAN = IBAN,
                birthdate = DateTime.Parse(dataNasc),
                morada = morada,
            };

            /* Tenta inserir na Base de Dados */
            try
            {
                db.Utilizadors.Add(u);
                db.Funcionarios.Add(f);

                db.SaveChanges();
            }
            catch ( DbEntityValidationException ex)
            {
                info = ex.ToString();
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            } 

            info =  "Funcionário Registado Com Sucesso";

            return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
        }

        /* Página Remover Funcionário */
        public ActionResult RemoverFuncForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;
            return View();
        }

        /* Método que remove o Funcionário */ 
        public ActionResult RemoveFunc(String email, String username, String emailF)
        {
            ViewBag.email = email;
            ViewBag.username = username;
            String info = "";

            Regex mail = new Regex(".*@(hotmail|gmail|outlook).com");

            if( !mail.IsMatch(emailF))
            {
                info = "Email Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            try
            {
                var user = (from u in db.Utilizadors
                            where u.email == emailF && u.estatuto == "F"
                            select u).SingleOrDefault();
                
                var func = (from f in db.Funcionarios
                            where f.Utilizador_email == emailF
                            select f).SingleOrDefault();

                db.Funcionarios.Remove(func);
                db.Utilizadors.Remove(user);
                db.SaveChanges();
            } catch ( ArgumentNullException ) {
                info = "Funcionário Inválido";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            } catch ( Exception) {
                info = "Não Foi Possível Remover o Funcionário";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            info = "Funcionário Removido Com Sucesso";
            return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
        }

        /* Página Para Alterar o Serviço */
        public ActionResult AltServicoForm(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            var serv = from s in db.Servicoes
                       select s;

            return View(serv);
        }

        /* Método Que Altera os Dados Do Serviço Escolhido */
        public ActionResult AltDadosServico(String email, String username, int servico,
            String descricao, String preco, String precoSProds)
        {
            String info = "";

            /* Validar Dados de Input */
            Regex p = new Regex("[0-9]+[,][0-9]{2}");

            if( !p.IsMatch(preco) )
            {
                info = "Preço Normal Errado";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            if( !p.IsMatch(precoSProds) )
            {
                info = "Preço Sem Produtos Errado";
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            try
            {
                var serv = (from s in db.Servicoes
                         where s.id_servico == servico
                         select s).Single();

                serv.descricao = descricao;
                float pN = float.Parse(preco);
                serv.preco = pN;
                float pSP = float.Parse(precoSProds);
                serv.precoSProduto = pSP;

                db.SaveChanges();
            } catch ( Exception e ) {
                info = e.ToString();
                return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
            }

            info = "Serviço Atualizado";
            return RedirectToAction("Index", "Administrador", new { email = email, username = username, info = info });
        }

        /* Página Que Devolve Todos Os Formulários de Satisfação */
        public ActionResult consFormSatisfacao(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            var forms = from f in db.Formulario_de_Satisfacao
                        where f.pendente == "N"
                        select f;

            return View(forms);
        }
    }
}