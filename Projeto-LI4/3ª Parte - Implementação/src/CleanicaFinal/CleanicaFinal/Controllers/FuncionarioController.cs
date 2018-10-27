using CleanicaFinal.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Web.Mvc;

namespace CleanicaFinal.Controllers
{
    public class FuncionarioController : Controller
    {
        CleanicaContext db = new CleanicaContext();
        FuncServContext fs = new FuncServContext();

        public ActionResult Index(String email, String username, String info)
        {
            ViewBag.username = username;
            ViewBag.email = email;
            ViewBag.info = info;

            DateTime[] dateTimes = new DateTime[2];
            dateTimes = GetDatasHoje();

            DateTime d1 = dateTimes[0];
            DateTime d2 = dateTimes[1];

            var servs = (from t in fs.Funcionario_ServicosRealizados
                         where t.Funcionario_Utilizador_email == email
                         select t);

            List<ServicosRealizado> sR = new List<ServicosRealizado>();

            foreach(var item in servs)
            {
                ServicosRealizado s = (from t in db.ServicosRealizados
                                       where t.id_ServicosRealizados == item.ServicosRealizados_id_ServicosRealizados && 
                                       t.data >= d1 && t.data <= d2 /*&& t.pendente == "S"*/
                                       select t).FirstOrDefault();

                if ( s != null) sR.Add(s);
            }
            return View(sR);
        }

        /* Página Formulário de Realização */
        public ActionResult FormRealizacao(String email, String username, String cliente, String data)
        {
            ViewBag.email = email;
            ViewBag.username = username;
            ViewBag.cliente = cliente;
            ViewBag.data = data;

            return View();
        }

        /* Submeter Formulário de Realização */
        public ActionResult PreencheFormServ(String email, String username, String cliente, 
            String data, String duracao, String obs)
        {
            String info = "";
            Regex dur = new Regex("[0-9]$");

            if (!dur.IsMatch(duracao))
            {
                info = "O campo Duração deve ser preenchido com um valor entre 1 e 5";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            int d = Int32.Parse(duracao);

            if (d <= 0 || d >= 6)
            {
                info = "O campo Duração deve ser preenchido com um valor entre 1 e 5";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            try
            {
                Formulario_de_Realizacao fR = new Formulario_de_Realizacao()
                {
                    Cliente_Utilizador_email = cliente,
                    FuncionarioUtilizador_email = email,
                    duracao = d,
                    observacoes = obs
                };

                Formulario_de_Satisfacao fS = new Formulario_de_Satisfacao()
                {
                    Cliente_Utilizador_email = cliente,
                    Funcionario_Utilizador_email = email,
                    sugestoes = null,
                    pontuacao = null,
                    pendente = "S"
                };

                db.Formulario_de_Realizacao.Add(fR);
                db.Formulario_de_Satisfacao.Add(fS);

                db.SaveChanges();
            }
            catch (Exception)
            {
                info = "Não Foi Possível Registar o Formulário. Tente Mais Tarde.";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            info = "Formulário Registado Com Sucesso";
            return RedirectToAction("Index", "Funcionario", new { email, username, info });
        }

        public ActionResult ConsultaHistorico(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            var servs = (from t in fs.Funcionario_ServicosRealizados
                         where t.Funcionario_Utilizador_email == email
                         select t);

            DateTime[] dateTimes = new DateTime[2];
            dateTimes = GetDatasHoje();
            DateTime d1 = dateTimes[0];

            List<ServicosRealizado> sR = new List<ServicosRealizado>();

            foreach (var item in servs)
            {
                ServicosRealizado s = (from t in db.ServicosRealizados
                                       where t.id_ServicosRealizados == item.ServicosRealizados_id_ServicosRealizados &&
                                       t.data < d1 && t.pendente == "N"
                                       select t).FirstOrDefault();

                if (s != null) sR.Add(s);
            }
            return View(sR);
        }

        /* Página Alterar Dados Funcionário (Form) */
        public ActionResult AlterarDadosFunc(String email, String username)
        {
            ViewBag.email = email;
            ViewBag.username = username;

            var user = (from t in db.Utilizadors
                        where t.email == email
                        select t).First();

            var func = (from t in db.Funcionarios
                        where t.Utilizador_email == email
                        select t).First();

            ViewBag.user = user.username;
            ViewBag.pass = user.password;
            ViewBag.tel = func.telemovel;
            ViewBag.IBAN = func.IBAN;
            ViewBag.morada = func.morada;

            return View();
        }

        /* Submeter Alteração de Dados */
        public ActionResult AlteraDadosFunc(String email, String username, String usernameN, 
            String password, String telemovel, String IBAN, String morada)
        {
            ViewBag.username = username;
            ViewBag.email = email;
            String info = "";

            Regex tlmv = new Regex("9[1236][0-9]{7}$");
            Regex iban = new Regex("PT50[0-9]{21}$");

            if (!tlmv.IsMatch(telemovel))
            {
                info = "Formato Inválido Para o Campo Telemóvel";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            if (!iban.IsMatch(IBAN)){
                info = "Formato Inválido Para o Campo IBAN";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            try
            {
                var query = (from t in db.Funcionarios
                            where t.Utilizador_email == email
                            select t).First();

                /* Update na Base de Dados */ 
                query.morada = morada;
                query.IBAN = IBAN;
                query.telemovel = Int32.Parse(telemovel);
                

                var query2 = (from t in db.Utilizadors
                             where t.email == email
                             select t).First();

                /* Update na Base de Dados */ 
                query2.username = username;
                query2.password = password;

                db.SaveChanges();

            }
            catch (Exception)
            {
                info = "Não Foi Possível Atualizar Os Dados";
                return RedirectToAction("Index", "Funcionario", new { email, username, info });
            }

            info = "Dados Alterados Com Sucesso";
            return RedirectToAction("Index", "Funcionario", new { email, username, info });
        }

        /****************** AUXILIAR ******************/

        private DateTime[] GetDatasHoje()
        {
            DateTime[] dateTimes = new DateTime[2];

            DateTime d = DateTime.Now;
            String year = d.Year.ToString();
            String month = d.Month.ToString();
            String day = d.Day.ToString();
            int hour = d.Hour;
            int minute = d.Minute;
            int second = d.Second;

            TimeSpan timeSpan = new TimeSpan(0, -hour, -minute, -second);
            TimeSpan timeSpan2 = new TimeSpan(0, 23, 59, 59);

            DateTime dA = d.Add(timeSpan);
            DateTime dB = dA.Add(timeSpan2);

            dateTimes[0] = dA;
            dateTimes[1] = dB;

            return dateTimes;
        }
    }
}
 