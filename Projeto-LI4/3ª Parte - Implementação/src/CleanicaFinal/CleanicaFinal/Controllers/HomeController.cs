using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Web;
using System.Web.Mvc;

namespace CleanicaFinal.Controllers
{
    public class HomeController : Controller
    {
        CleanicaContext db = new CleanicaContext();

        public ActionResult Index(String info = "Já Está Registado? Faça o Login Aqui")
        {
            ViewBag.info = info;
            return View();
        }

        [HttpPost]
        public ActionResult RegistarCliente(String email, String username, String password, String telemovel, String morada)
        {
            String info = "";
            Regex e = new Regex(".*@(hotmail|gmail|outlook).com$");
            Regex t = new Regex("9[1236][0-9]{7}$");

            /* Verifica se o email é válido */ 
            if ( e.IsMatch(email) == false )
            {
                info = "Email Inválido";
                return RedirectToAction("Index", "Home", new { info });
            }

            /* Verifica se o telemóvel é válido */
            if ( t.IsMatch(telemovel) == false)
            {
                info = "Telemóvel Inválido";
                return RedirectToAction("Index", "Home", new { info });
            }
           
            Utilizador u = new Utilizador
            {
                email = email,
                username = username,
                password = password,
                estatuto = "C"
            };

            Cliente c = new Cliente
            {
                Utilizador_email = email,
                telemovel = Int32.Parse(telemovel),
                morada = morada,
            };

            /* Tenta inserir na Base de Dados */ 
            try
            {
                db.Utilizadors.Add(u);
                db.Clientes.Add(c);
            
                db.SaveChanges();
            } catch ( Exception ) {
                info = "O email indicado já se encontra registado na base de dados";
                return RedirectToAction("Index", "Home", new { info });
            }

            info = "Cliente Registado Com Sucesso";

            return RedirectToAction("Index", "Home", new { info });
        }

        public ActionResult Login(String email, String password)
        {
            String info = "", em = "", pass = "", est = "", username = "";

            var query = from t in db.Utilizadors
                        where t.email == email
                        select t;

            foreach (var item in query)
            {
                em = item.email;
                pass = item.password;
                est = item.estatuto;
                username = item.username;
            }

            if (password == pass)
            {
                if (est == "C")
                {
                    info = "Login Como Cliente Bem Sucedido";
                    return RedirectToAction("Index", "Cliente", new { email, username, info });
                }
                else if (est == "A")
                {
                    info = "Login Como Administrador Bem Sucedido";
                    return RedirectToAction("Index", "Administrador", new { email, username, info });
                }
                else
                {
                    info = "Login Como Funcionário Bem Sucedido";
                    return RedirectToAction("Index", "Funcionario", new { email, username, info });
                }
            }
            else
            {
                info = "Email ou Password Inválidos";
                return RedirectToAction("Index", "Home", new { email, username, info });
            }
        }
    }
}