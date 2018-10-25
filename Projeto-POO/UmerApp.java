
/**
 * Esta classe é a responsável pela apresentação do menu no ecrã e pelo
 * tratamento de todas as opções apresentadas por este.
 * Esta classe tem como variáveis a empresa Umer e os três menus
 * principais da aplicação, gerados a partir da classe Menu.
 *
 * @author David Sousa
 * @author Carlos Pedrosa
 * @author Manuel Sousa
 */

import java.io.*;
import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Scanner;
import java.io.PrintWriter;

public class UmerApp {
    
    /** Variáveis de Instância */
    private static Umer umer;
    private static Menu menuInicial, menuCliente, menuMotorista;
    
    /** O construtor é privado, pois não queremos instâncias da mesma. */
    private UmerApp() {}
    
    /** 
     * Método main da aplicação;
     * Aqui é iniciada a aplicação, e apresentado no ecrã uma série de opções as
     * quais o Utilizador pode selecionar;
     * É devolvida também uma excepção se não for possível guardar o estado da
     * aplicação num ficheiro.
     */
    public static void main(String[] args) throws FileNotFoundException {   
        fazMenus();
        umer = new Umer();
        //carregarTestes();
        do {
            menuInicial.executa();
            switch(menuInicial.getOpcao()) {
                case 1: iniciarSessao();
                        break;
                case 2: criarUtilizador();
                        break;
                case 3: criarTaxi();
                        break;
                case 4: adicionarMotorista_Viatura();
                        break;
                case 5: top5DesviosMotoristas();
                        break;
                case 6: totalFaturadoViatura();
                        break;
                case 7: top10ClientesMaiorGasto();
                        break;
                case 8: carregarDados();
                        break;
                case 9: guardarDados();
                        break;
            }
        } while (menuInicial.getOpcao() != 0);
    }

    /**
     * Cria os diferentes menus da classe, conforme as opções desejadas.
     */
    private static void fazMenus() {
        String[] inicial = {"Inicial", "Iniciar Sessão", "Criar Utilizador",
                           "Adicionar Viaturas", "Adicionar Motoristas a Viaturas",
                           "Listagem dos 5 Motoristas com mais desvios", 
                           "Total faturado por uma certa Viatura",
                           "Listagem dos 10 Clientes que mais gastam",
                           "Carregar dados de ficheiro", "Guardar dados em ficheiro"};
        
        String[] cliente = {"Cliente", "Fazer uma Viagem", "Listagem das Viagens Efetuadas"};
        
        String[] motorista = {"Motorista", "Listagem das Viagens Efetuadas"};
        
        menuInicial = new Menu(inicial);
        menuCliente = new Menu(cliente);
        menuMotorista = new Menu(motorista);
    }

    /** 
     * Menu de início de sessão, que conforme o utilizador, executa o Menu de
     * Cliente ou o Menu de Motorista; Se os dados de um certo utilizador não
     * corresponderem com o que está na base de dados, é imprimida uma excepção
     * e o utilizador é devolvido ao Menu Inicial. 
     */
    private static void iniciarSessao() {
        Actor aux = new Actor();
        Scanner input = new Scanner(System.in);
        
        System.out.print('\u000C');
        System.out.println("------> Bem-vindo Utilizador da UMeR <------");
        System.out.println("-> Iniciar sessão: ");
        System.out.print("E-mail do Utilizador: ");
        String email = input.nextLine();
        System.out.print("Password: ");
        String pass = input.nextLine();
        
        try {
            umer.verificaDados(email, pass);
        }
        catch (ErroLogException erro) {
            System.out.println(erro.getMessage()); //Recorre a Exception para dar msg de erro
            return;
        }
        
        aux = umer.getActores().get(email);

        if (aux instanceof Cliente) {
            do {
                Cliente c = (Cliente) aux;
                menuCliente.executa();
                switch(menuCliente.getOpcao()) {
                    case 1: fazerViagem(c);
                            break;
                    case 2: getInfoViagensComDatas(c);
                            break;
                    case 0: break;
                    default: System.out.println("\n Opção Inválida. Tente novamente.");
                             break;
                }
            } while (menuCliente.getOpcao() != 0);
           
        }

        if (aux instanceof Motorista) {
            do {
                Motorista m = (Motorista) aux;
                menuMotorista.executa();
                switch(menuMotorista.getOpcao()) {
                    case 1: getInfoViagensComDatas(m);
                            break;
                    case 0: break;
                    default: System.out.println("\n Opção Inválida. Tente novamente.");
                             break;
                }
            } while(menuMotorista.getOpcao() != 0);
        }
    }
    
    /**
     * Método que cria um Utilizador novo no sistema;
     * Este utilizador é depois colocado na Lista de Actores do sistema.
     */
    private static void criarUtilizador() {
        Scanner input = new Scanner(System.in);
        ArrayList<Viagem> viagens = new ArrayList<Viagem>();
        Taxi t = new Taxi();
        Ponto2D p = new Ponto2D(0,0);
        HashMap<String,Actor> actores = umer.getActores();
        String nome, email, pass, morada, tipoUtilizador;
           
        System.out.print('\u000C');
        System.out.println("------> Novo Utilizador <------");
        System.out.print("Nome: ");
        nome = input.nextLine();
        System.out.print("Email: ");
        email = input.nextLine();
        System.out.print("Password: ");
        pass = input.nextLine();
        System.out.print("Morada: ");
        morada = input.nextLine();
        System.out.println("Data de Nascimento: ");
        System.out.print("-> Dia: ");
        String dia = input.nextLine();
        System.out.print("-> Mes: ");
        String mes = input.nextLine();
        System.out.print("-> Ano: ");
        String ano = input.nextLine();
        
        int d = 0, m = 0, a = 0;
        try {
            d = Integer.parseInt(dia);
            m = Integer.parseInt(mes);
            a = Integer.parseInt(ano);
        } catch (NumberFormatException erro) {
            System.out.println("Data de nascimento inválida!");
            return;
        }
        Calendar cal = new GregorianCalendar(a, m, d);
        
        System.out.println("Tipo de Utilizador (Cliente ou Motorista)");
        System.out.println("1 - Cliente");
        System.out.println("2 - Motorista");
        System.out.print("Opção: ");
        tipoUtilizador = input.nextLine();
        
        int op; 
        try {
            op = Integer.parseInt(tipoUtilizador);
        } catch (NumberFormatException erro) {
            System.out.println(erro.toString());
            return;
        }
        
        if (tipoUtilizador.equals("1")) {
            try {
                Cliente c = new Cliente(email, nome, pass, morada, cal, p, t, viagens, false);
                umer.criarUtilizador(c);
            } catch (ActorRepetidoException erro) {
                System.out.println(erro.toString());
                return;
            }
        }
        else if (tipoUtilizador.equals("2")) {
            try {
                Motorista mt = new Motorista(email, nome, pass, morada, cal, 1, 0, 0, true, viagens, 0);
                umer.criarUtilizador(mt);
            } catch (ActorRepetidoException erro) {
                System.out.println(erro.toString());
                return;
            }
        }
        else {
            System.out.println("Opção Inválida!");
            return;
        }
        
        System.out.println("");
        System.out.println("Utilizador criado com sucesso");
    }

    /** 
     * Método que cria uma Viatura nova no sistema;
     * Esta viatura é depois colocada na Lista de Viaturas do sistema.
     */
    private static void criarTaxi() {
        Scanner input = new Scanner(System.in);
        Ponto2D coords = new Ponto2D(0,0);
        ArrayList<String> emailsMotoristas = new ArrayList<String>();
        String tipoTaxi, velMedia, precoKm, factorFiabilidade;
        
        //Copia todos os emails dos motoristas para uma outra lista auxiliar
        for (Motorista m: umer.getMotoristas()) emailsMotoristas.add(m.getEmail());
        
        System.out.print('\u000C');
        System.out.println("------> Novo Taxi <------");
        System.out.println("Tipo de Viatura:");
        System.out.println("1 - Carro Ligeiro");
        System.out.println("2 - Carrinha de nove lugares");
        System.out.println("3 - Moto");
        System.out.print("Opcao: ");
        tipoTaxi = input.next();
        
        int op; 
        try {
            op = Integer.parseInt(tipoTaxi);
        }
        catch (NumberFormatException erro) {
            System.out.println("Opcao Invalida!");
            return;
        }
        
        if (op == 1 || op == 2 || op == 3) {
            System.out.println("");
            System.out.print("Velocidade media por Km -> ");
            velMedia = input.next();
            System.out.print("Preço por Km -> ");
            precoKm = input.next();
            System.out.print("Factor de Fiabilidade da Viatura -> ");
            factorFiabilidade = input.next();
        
            double aux1, aux2; int aux3;
            try {
                aux1 = Double.parseDouble(velMedia);
                aux2 = Double.parseDouble(precoKm);
                aux3 = Integer.parseInt(factorFiabilidade);
            }
            catch (NumberFormatException erro) {
                System.out.println(erro.toString());
                return;
            }
            
            System.out.println("Com ou sem lista de reservas? (Com -> 1 / Sem -> 0)");
            System.out.print("Opcao: ");
            String rev = input.next();
            
            try {
                op = Integer.parseInt(rev);
            }
            catch (NumberFormatException erro) {
                System.out.println(erro.toString());
                return;
            }
            
            System.out.println("");
            System.out.println("Pretende adicionar um Motorista ao Taxi? (Sim -> 1 / Nao -> 0)");
            System.out.print("Opcao: ");
            String add = input.next();
            
            int choice;
            try {
                choice = Integer.parseInt(add);
            }
            catch (NumberFormatException erro) {
                System.out.println(erro.toString());
            }
            
            System.out.println("");
            System.out.println("Qual a matrícula para a Viatura?");
            System.out.print("Opcao: ");
            String matricula = input.next();
            
            //Fazer try/catch com matricula?
            
            ArrayList<Cliente> reservas = new ArrayList<Cliente>();
            ArrayList<Viagem> viagens = new ArrayList<Viagem>();
            Motorista mAux = new Motorista();
            if (add.equals("1")) {
                System.out.println("Qual o motorista que deseja para este novo Taxi? (Fornecer Email!)");
                System.out.println(emailsMotoristas.toString());
                System.out.println("");
                System.out.print("Email: ");
                String email = input.next();
            
                while (!emailsMotoristas.contains(email)) {
                    System.out.println("");
                    System.out.println("Email fornecido nao pertence a lista. Por favor, tente novamente.");
                    System.out.print("Email: ");
                    email = input.next();
                }
            
                //Vai ao HashMap<String,Actor> buscar o motorista cujo email foi dado pelo input do utilizador
                mAux = (Motorista) umer.getActores().get(email);
            }
            
            if (rev.equals("1")) {
                if (add.equals("1")) {
                    umer.setTaxiVazio(mAux);
                    if (tipoTaxi.equals("1")) {
                        Carros_Ligeiros_comReservas novo = new Carros_Ligeiros_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                    if (tipoTaxi.equals("2")) {
                        Carrinhas_comReservas novo = new Carrinhas_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                    if (tipoTaxi.equals("3")) {
                        Motos_comReservas novo = new Motos_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                }
                else if (add.equals("0")) { 
                    if (tipoTaxi.equals("1")) {
                        Carros_Ligeiros_comReservas novo = new Carros_Ligeiros_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                    if (tipoTaxi.equals("2")) {
                        Carrinhas_comReservas novo = new Carrinhas_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                    if (tipoTaxi.equals("3")) {
                        Motos_comReservas novo = new Motos_comReservas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens, reservas);
                    }
                }
                else {
                    System.out.println("Opção Inválida!");
                    return;
                }
            }
            else if (rev.equals("0")) {
                if (add.equals("1")) {
                    umer.setTaxiVazio(mAux);
                    if (tipoTaxi.equals("1")) {
                        Carros_Ligeiros novo = new Carros_Ligeiros(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                    if (tipoTaxi.equals("2")) {
                        Carrinhas novo = new Carrinhas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                    if (tipoTaxi.equals("3")) {
                        Motos novo = new Motos(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                }
                else if (add.equals("0")) { 
                    if (tipoTaxi.equals("1")) {
                        Carros_Ligeiros novo = new Carros_Ligeiros(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                    if (tipoTaxi.equals("2")) {
                        Carrinhas novo = new Carrinhas(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                    if (tipoTaxi.equals("3")) {
                        Motos novo = new Motos(matricula, mAux, aux1, aux2, aux3, 0, coords, viagens);
                    }
                }
                else {
                    System.out.println("Opção Invalida!");
                    return;
                }
            }
            else {
                System.out.println("Opção Invalida!");
                return;
            }
        }
        else {
            System.out.println("Opção Invalida!");
            return;
        }
        
        System.out.println("");
        System.out.println("Viatura criada com sucesso!");
    }
    
    /** 
     * Método que adiciona um determinado motorista selecionado pelo utilizador,
     * a uma Viatura presente no sistema.
     */
    private static void adicionarMotorista_Viatura() {
        Scanner input = new Scanner(System.in);
        ArrayList<String> emails = umer.getEmailsMotoristas();
        ArrayList<String> matriculas = umer.getMatriculasTaxis();
        
        System.out.print('\u000C');
        System.out.println("------> Adicionar Motorista a uma Viatura <------");
        System.out.println("Selecione qual motorista pretende adicionar: ");
        System.out.println("");
        System.out.println(emails.toString());
        System.out.println("");
        System.out.print("Opção: ");
        String motorista = input.next();
        
        while (!emails.contains(motorista)) {
            System.out.println("");
            System.out.println("Email não é válido. Por favor, tente novamente.");
            System.out.print("Opção: ");
            motorista = input.next();
        }
        
        System.out.println("");
        System.out.println("Qual a viatura que pretende adicionar? (Viatura identificada pela sua matrículas)");
        System.out.println("");
        System.out.println(matriculas.toString());
        System.out.println("");
        System.out.print("Opção: ");
        String matriculaTaxi = input.next();
        
        while (!matriculas.contains(matriculaTaxi)) {
            System.out.println("");
            System.out.println("A Matrícula não é válida. Por favor, tente novamente.");
            System.out.print("Opção: ");
            matriculaTaxi = input.next();
        }
        
        Taxi t = umer.getTaxiByMatricula(matriculaTaxi);
        Motorista m = umer.getMotoristaByEmail(motorista);
        int index = umer.getTaxis().indexOf(t);
        
        t.setMotorista(m);
        
        ArrayList<Taxi> auxTaxis = umer.getTaxis();
        auxTaxis.set(index, t);
        
        umer.setTaxis(auxTaxis);
        
        System.out.println("");
        System.out.println("Motorista adicionado com sucesso!");
    }
    
    /** 
     * Método que trata de imprimir para o ecrã a Lista com os 5 Motoristas com
     * mais desvios no sistema.
     */
    private static void top5DesviosMotoristas() {
        ArrayList<Actor> actores = umer.top5Desvios();
        ArrayList<Motorista> motoristas = new ArrayList<Motorista>();
        ArrayList<Integer> desvios = new ArrayList<Integer>();
        
        for (Actor a : actores) {
            Motorista aux = (Motorista) a.clone();
            motoristas.add(aux);
        }

        for (Motorista m : motoristas) {
            desvios.add(m.contaDesvios());
        }
        
        System.out.print('\u000C');
        System.out.println("Lista dos 5 Motoristas com mais desvios: ");
        for (int i = 0; i < motoristas.size(); i++) {
            Motorista m = motoristas.get(i);
            Actor act = (Actor) m;
            
            System.out.println("-> Motorista: " + act.getEmail());
            System.out.println("Total de Desvios: " + m.contaDesvios());
            System.out.println("");
        }
    }

    /** 
     * Método que imprime no ecrã o total faturado por uma viatura num
     * determinado período.
     */
    private static void totalFaturadoViatura() {
        Scanner input = new Scanner(System.in);
        ArrayList<String> matriculas = umer.getMatriculasTaxis();
        String diaI, mesI, anoI, diaF, mesF, anoF;
        
        System.out.print('\u000C');
        System.out.println("Entre que datas pretende saber o total faturado?");
        System.out.println("Data Inicial: ");
        System.out.print("-> Dia: ");
        diaI = input.nextLine();
        System.out.print("-> Mês: ");
        mesI = input.nextLine();
        System.out.print("-> Ano: ");
        anoI = input.nextLine();
        System.out.println("");
        System.out.println("Data Final: ");
        System.out.print("-> Dia: ");
        diaF = input.nextLine();
        System.out.print("-> Mês: ");
        mesF = input.nextLine();
        System.out.print("-> Ano: ");
        anoF = input.nextLine();
        
        int dia1 = 0, mes1 = 0, ano1 = 0, dia2 = 0, mes2 = 0, ano2 = 0;
        try {
            dia1 = Integer.parseInt(diaI);
            mes1 = Integer.parseInt(mesI);
            ano1 = Integer.parseInt(anoI);
            dia2 = Integer.parseInt(diaF);
            mes2 = Integer.parseInt(mesF);
            ano2 = Integer.parseInt(anoF);
        }
        catch (NumberFormatException e) {
            System.out.println("Opção Inválida!");
            return;
        }
        
        GregorianCalendar dataInicial = new GregorianCalendar(ano1, mes1, dia1);
        GregorianCalendar dataFinal = new GregorianCalendar(ano2, mes2, dia2);
        
        System.out.println("------> Total Faturado Por uma Viatura <------");
        System.out.println("De qual viatura pretende saber o total faturado?");
        System.out.println("");
        System.out.println(matriculas.toString());
        System.out.println("");
        System.out.print("Opção: ");
        String matriculaTaxi = input.next();
        
        while (!matriculas.contains(matriculaTaxi)) {
            System.out.println("");
            System.out.println("A Matricula não é válida. Por favor, tente novamente.");
            System.out.print("Opção: ");
            matriculaTaxi = input.next();
        }
        
        Taxi aux = umer.getTaxiByMatricula(matriculaTaxi);
        
        double totalFaturado = aux.getTotalFaturadoData(dataInicial, dataFinal);
        
        System.out.println("");
        System.out.println("Total Faturado (" + matriculaTaxi + "): " + totalFaturado);
    }
    
    /** Método que trata de imprimir para o ecrã a Lista dos 10 Clientes do
     * sistema que mais dinheiro gastaram.
     */
    private static void top10ClientesMaiorGasto() {
        ArrayList<Actor> actores = umer.top10Clientes();
        ArrayList<Cliente> clientes  = new ArrayList<Cliente>();
        ArrayList<Double> despesas = new ArrayList<Double>();
        
        for (Actor a : actores) {
            Cliente aux = (Cliente) a.clone();
            clientes.add(aux);
        }

        for (Cliente c: clientes) {
            despesas.add(c.despesaTotal());
        }
        
        System.out.print('\u000C');
        System.out.println("Lista dos 10 Clientes que mais gastam: ");
        for (int i = 0; i < clientes.size(); i++) {
            Cliente c = clientes.get(i);
            Actor act = (Actor) c;
            
            System.out.println("-> Cliente: " + act.getEmail());
            System.out.println("Total gasto: " + c.despesaTotal());
            System.out.println("");
        }
    }

    /**
     * Carrega os dados para a aplicação.
     */
    private static void carregarDados() {
        System.out.print('\u000C');
        System.out.println ("--- Carregar Dados ---\n");
        try {
            umer = Umer.startApp();
        }
        catch (IOException e) {
            System.out.println("Não consegui ler os dados!\nErro de leitura.");
            return;
        } 
        catch (ClassNotFoundException e) {
            System.out.println("Não consegui ler os dados!\nFicheiro com formato desconhecido.");
            return;
        }
        catch (ClassCastException e) {
            System.out.println("Não consegui ler os dados!\nErro de formato.");
            return;
        }

        System.out.println ("Dados carregados com sucesso");
    }
    
    /** 
     * Guarda os dados da aplicação em ficheiro.
     */
    private static void guardarDados() {
        System.out.print('\u000C');
        System.out.println ("--- Guardar Dados ---\n");
        try {
            umer.saveApp("estado.txt");
            umer.log("log.txt", true);
        }
        catch (IOException e) {
            System.out.println("Não consegui gravar os dados!");
            return;
        }
             
        System.out.println ("Dados guardados com sucesso");
    }

    /** 
     * Método que executa uma viagem para um certo Cliente; 
     * Aqui o Cliente tem diferentes opções em relação à sua viagem; 
     * Se este Cliente já estiver em alguma Lista de Reservas, não é possível realizar a viagem;
     * Conforma a opção do Cliente, ele pode escolher reservar um lugar num
     * certo Táxi, se este tiver essa opção, ou fazer logo a viagem.
     * @param c
     */
     private static void fazerViagem(Cliente c) {
        Taxi aux = new Taxi();
        Scanner input = new Scanner(System.in);
        boolean taxiProximo;
        
        System.out.print('\u000C');
        
        /* Verifica se o Cliente consta em alguma lista de reservas. 
           Se sim, não é possível realiza a viagem. */
        if (umer.existeEmReservas(c)) {
            System.out.println("");
            System.out.println("Não é possível realizar uma viagem de momento.");
            System.out.println("Já está numa lista de reservas de uma viatura. Por favor, tente mais tarde.");
            return;
        }

        System.out.println("------> Fazer Viagem com a UMeR! <------");
        System.out.println("Indique a sua localização (Coordenadas 'x' e 'y'): ");
        System.out.print("Coordenada 'x' -> ");
        String xInicio = input.next();
        System.out.print("Coordenada 'y' -> ");
        String yInicio = input.next();
        
        double coordXInicio, coordYInicio;
        try {
            coordXInicio = Double.parseDouble(xInicio);
            coordYInicio = Double.parseDouble(yInicio);
        }
        catch (NumberFormatException erro) {
            System.out.println(erro.toString());
            return;
        }
        
        Ponto2D coordsCliente = new Ponto2D(coordXInicio, coordYInicio);
        
        System.out.println("");
        System.out.println("-> Deseja selecionar um Taxi ou viajar no Taxi mais próximo da sua localização? \n(0 -> Viajar no mais próximo / 1 -> Escolher Taxi)");
        System.out.print("Opção: ");
        String opcao = input.next();
        
        int op;
        try {
            op = Integer.parseInt(opcao);
        }
        catch (NumberFormatException erro) {
            System.out.println(erro.toString());
            return;
        }
        
        System.out.println("");
        System.out.println("-> Qual deseja ser o seu destino?");
        System.out.print("Coordenada 'x' -> ");
        String xDest = input.next();
        System.out.print("Coordenada 'y' -> ");
        String yDest = input.next();
        
        double coordXDestino, coordYDestino;
        try {
            coordXDestino = Double.parseDouble(xDest);
            coordYDestino = Double.parseDouble(yDest);
        }
        catch (NumberFormatException erro) {
            System.out.println(erro.toString());
            return;
        }
       
        Ponto2D coordsDestinoCliente = new Ponto2D(coordXDestino, coordYDestino);
        
        //Viajar no mais proximo
        if (op == 0) {
            taxiProximo = true;
            //Selecionar o taxi mais proximo das coordenadas que me foram fornecidas
            try { aux = umer.selecionaTaxiDisponivelMaisProximo(coordsCliente); } 
            catch (ArrayListVazioException erro) { System.out.println(erro.toString()); }
            
            if (aux instanceof Carros_Ligeiros_comReservas) { 
                Carros_Ligeiros_comReservas revs = (Carros_Ligeiros_comReservas) aux;
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                    
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem.");
                
                return;
            }
            else if (aux instanceof Carrinhas_comReservas) { 
                Carrinhas_comReservas revs = (Carrinhas_comReservas) aux;
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                    
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem.");
                
                return;
            }
            else if (aux instanceof Motos_comReservas) { 
                Motos_comReservas revs = (Motos_comReservas) aux;
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                    
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem.");
                
                return;
            }
        }
        //Escolher um taxi
        else if (op == 1) {
            taxiProximo = false;
            ArrayList<String> emails = new ArrayList<String>();
            emails = umer.getEmailsMotoristas();
            String motorista = new String();
            
            System.out.println("De entre os Motoristas disponiveis, selecione o desejado: ");
            System.out.println(emails.toString());
            System.out.print("Opção: ");
            motorista = input.next();
            
            while (!emails.contains(motorista)) {
                System.out.println("");
                System.out.println("Email não é válido. Por favor, tente novamente.");
                System.out.print("Opção: ");
                motorista = input.next();
            }
            
            //Usa o metodo getTaxiByEmail para obter o Taxi cujo Motorista tem o Email fornecido.
            aux = umer.getTaxiByEmail(motorista);
            
            if (aux instanceof Carros_Ligeiros_comReservas) { 
                Carros_Ligeiros_comReservas revs = (Carros_Ligeiros_comReservas) aux; 
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem.");
                
                return;
            }
            else if (aux instanceof Carrinhas_comReservas) { 
                Carrinhas_comReservas revs = (Carrinhas_comReservas) aux;
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem."); 
                
                return;
            }
            else if (aux instanceof Motos_comReservas) { 
                Motos_comReservas revs = (Motos_comReservas) aux;
                ArrayList<Taxi> taxistas = umer.getTaxis();
                int indexTaxi = umer.getTaxis().indexOf(aux);
                int indexMotorista = umer.getMotoristas().indexOf(aux.getMotorista());
                ArrayList<Cliente> newReservas = revs.getReservas();
                
                newReservas.add(c);
                revs.setReservas(newReservas);
                
                if (newReservas.size() == 2) {
                    umer.fazerViagemReservada(umer.getInfoReservas(), newReservas);
                }
                else {
                    InfoReservas info = new InfoReservas(c, revs, coordsDestinoCliente, indexTaxi, indexMotorista, taxiProximo);
                    umer.setInfoReservas(info);
                    
                    taxistas.set(indexTaxi, revs);
                    umer.setTaxis(taxistas);
                }
                
                System.out.println("");
                System.out.println("Foi reservado o seu pedido no taxi.");
                System.out.println("Quando o taxi estiver disponível, será possível realizar a viagem.");

                return;
            }
        }
        else {
            System.out.println("Opção Inválida!");
            return;
        }
        
        double distTaxi_Cliente = Math.round(umer.calculaDistanciaViagem(aux.getCoords(), coordsCliente) * 100.0)/100.0;
        double tempoTaxi_Cliente = umer.calculaTempoViagem(aux.getCoords(), coordsCliente, aux);
        
        double tempoTaxi_ClienteMin = Math.round(tempoTaxi_Cliente * 60);
        System.out.println("");
        System.out.println("-> O Taxi demorará " + tempoTaxi_ClienteMin + " minutos a chegar à sua localização.");
        
        double distCliente_Destino = Math.round(umer.calculaDistanciaViagem(coordsCliente, coordsDestinoCliente) * 100.0)/100.0;
        double tempoCliente_Destino = umer.calculaTempoViagem(coordsCliente, coordsDestinoCliente, aux);
        
        double tempoCliente_DestinoMin = Math.round(tempoCliente_Destino * 60);
        System.out.println("Desde a sua localização até ao destino desejado demorará aproximadamente " + tempoCliente_DestinoMin + " minutos.");
        
        double custoEstimadoViagem = Math.round((umer.calculaCustoEstimadoViagem(coordsCliente, coordsDestinoCliente, aux)
                                     + umer.calculaCustoEstimadoViagem(aux.getCoords(), coordsCliente, aux)) * 100.0)/100.0;
        double custoRealViagem = custoEstimadoViagem;    
        
        double tempoEstimado = tempoTaxi_Cliente + tempoCliente_Destino;
        double tempoReal = tempoEstimado + (aux.getFactor() * tempoEstimado);
        double difTempos = tempoReal - tempoEstimado;
        
        double tempoEstimadoMin = Math.round(tempoEstimado * 60);
        System.out.println("");
        System.out.println("O custo estimado da viagem será de " + custoEstimadoViagem + " euros.");
        System.out.println("O tempo estimado da viagem será de " + tempoEstimadoMin + " minutos.");
            
        if (difTempos <= (0.25 * tempoEstimado)) {
            custoRealViagem = Math.round((custoRealViagem + (aux.getPrecoKm() * (difTempos * aux.getVelMedia()))) * 100.0)/100.0;
        }
            
        double tempoRealMin = Math.round(tempoReal * 60);
        System.out.println("");
        System.out.println("Devido a diversos factores, o tempo total de viagem será de " + tempoRealMin + " minutos.");
        System.out.println("O custo total da viagem será de " + custoRealViagem + " euros.");
            
        System.out.println("");
        System.out.println("-> Deseja efetuar a viagem? (Sim -> 1 / Não -> 0)");
        System.out.print("Opção: ");
        opcao = input.next();
           
        try {
            op = Integer.parseInt(opcao);
        }
        catch (NumberFormatException erro) {
            System.out.println(erro.toString());
            return;
        }
        
        //Fazer a viagem e efetuar todas as alteraçoes nos objetos
        if (op == 1) {
            ArrayList<Viagem> viagens = new ArrayList<Viagem>();
            Motorista m = aux.getMotorista();
            int indexMotorista = umer.getMotoristas().indexOf(m);
            int indexTaxi = umer.getTaxis().indexOf(aux);
            double cumpHorario = (tempoReal * 100)/tempoEstimado;
            
            System.out.println("Que nota deseja atribuir a esta Viagem? (Entre 0 e 10, ou (-1) caso não queira)");
            System.out.print("Nota: ");
            String nota = input.next();
            
            int n;
            try {
                n = Integer.parseInt(nota);
            }   
            catch (NumberFormatException erro) {
                System.out.println(erro.toString());
                return;
            }
            
            while ((n < 0 || n > 10) && n != -1) {
                System.out.println("");
                System.out.println("Nota invalida. Por favor, introduza novamente.");
                System.out.print("Nota: ");
                nota = input.next();
                n = Integer.parseInt(nota);
            }
            
            m.setKmsRealizados(distTaxi_Cliente + distCliente_Destino);
            m.setNViagensEfetuadas(m.getNViagensEfetuadas() + 1);
            m.setCumpHorario(cumpHorario);
            
            GregorianCalendar data = new GregorianCalendar();
            
            Viagem trip = new Viagem(aux.getMotorista(), coordsCliente.clone(), coordsDestinoCliente.clone(), 
                                     taxiProximo, aux.clone(), distTaxi_Cliente, distCliente_Destino, 
                                     custoEstimadoViagem, custoRealViagem, n, data);
            viagens = c.getViagensEfetuadas();
            viagens.add(trip.clone());
            
            c.setCoords(coordsDestinoCliente);
            c.setViagensEfetuadas(viagens);
            
            m.setViagensEfetuadas(viagens);
            m.setDisponivel(true);
            
            aux.setMotorista(m); //Poe o motorista devolta no Taxi correspondente, depois de todas as alteraçoes.
            aux.setCoords(coordsDestinoCliente);
            aux.aumentarTotalFaturado(custoRealViagem); //Adiciona o custoRealViagem ao total ja faturado pela viatura.
            
            ArrayList<Viagem> auxViagensTaxi = aux.getViagensEfetuadas();
            auxViagensTaxi.add(trip.clone());
            aux.setViagensEfetuadas(auxViagensTaxi);
            
            HashMap<String,Actor> auxActores = umer.getActores();
            auxActores.replace(c.getEmail(), c);
            auxActores.replace(m.getEmail(), m);
            umer.setActores(auxActores);
            
            ArrayList<Motorista> auxMotoristas = umer.getMotoristas();
            auxMotoristas.set(indexMotorista, m); //Substitui o Motorista m antigo pelo novo com as alteraçoes feitas.
            umer.setMotoristas(auxMotoristas);
            
            ArrayList<Taxi> auxTaxis = umer.getTaxis();
            auxTaxis.set(indexTaxi, aux);
            umer.setTaxis(auxTaxis);
        }
        else if (op == 0) return;
        else { 
            System.out.println("Opção Inválida!");
            return;
        }          
    }

    /** 
     * Imprime no ecrã uma Lista de Viagens de um certo Actor, entre determinadas datas.
     * @param user
     */
    private static void getInfoViagensComDatas(Actor user) {
        Scanner input = new Scanner(System.in);
        String diaI, mesI, anoI, diaF, mesF, anoF;
        System.out.print('\u000C');
        System.out.println("Entre quais datas pretende saber a listagem de viagens efetuadas?");
        System.out.println("Data Inicial: ");
        System.out.print("-> Dia: ");
        diaI = input.nextLine();
        System.out.print("-> Mês: ");
        mesI = input.nextLine();
        System.out.print("-> Ano: ");
        anoI = input.nextLine();
        System.out.println("");
        System.out.println("Data Final: ");
        System.out.print("-> Dia: ");
        diaF = input.nextLine();
        System.out.print("-> Mês: ");
        mesF = input.nextLine();
        System.out.print("-> Ano: ");
        anoF = input.nextLine();
        
        int dia1 = 0, mes1 = 0, ano1 = 0, dia2 = 0, mes2 = 0, ano2 = 0;
        try {
            dia1 = Integer.parseInt(diaI);
            mes1 = Integer.parseInt(mesI) - 1;
            ano1 = Integer.parseInt(anoI);
            dia2 = Integer.parseInt(diaF);
            mes2 = Integer.parseInt(mesF) - 1;
            ano2 = Integer.parseInt(anoF);
        }
        catch (NumberFormatException e) {
            System.out.println("");
            System.out.println("Opção Inválida!");
            return;
        }
        
        GregorianCalendar dataInicial = new GregorianCalendar(ano1, mes1, dia1);
        GregorianCalendar dataFinal = new GregorianCalendar(ano2, mes2, dia2);
        
        ArrayList<Viagem> auxDatas = new ArrayList<Viagem>();
        ArrayList<Viagem> auxInfo = new ArrayList<Viagem>();
        if (user instanceof Cliente) {
            Cliente aux = (Cliente) user;
            auxDatas = aux.getViagensEfetuadas();
        }
        else if (user instanceof Motorista) {
            Motorista aux = (Motorista) user;
            auxDatas = aux.getViagensEfetuadas();
        }
        
        for (Viagem v : auxDatas) {
            int d = v.getCalendar().get(Calendar.DAY_OF_MONTH);
            int m = v.getCalendar().get(Calendar.MONTH); 
            int a = v.getCalendar().get(Calendar.YEAR);
            GregorianCalendar dataAux = new GregorianCalendar(a, m, d);
            if ((dataAux.equals(dataInicial) || dataAux.equals(dataFinal)) || 
                v.getCalendar().before(dataFinal) && v.getCalendar().after(dataInicial)) {
                auxInfo.add(v.clone());
            }
        }
         
        System.out.println("");
        System.out.println(auxInfo.toString());
    }
    
    /** 
     * Método auxiliar que cria uma série de objetos para servir de teste na
     * aplicação; Estes objetos são, depois de criados, inseridos na base de
     * dados, de forma a poderem ser utilizador quando a aplicação é iniciada.
     */
    /*
    private static void carregarTestes() {
        String[] emails = {"motorista1@umer.com", "motorista2@umer.com", "motorista3@umer.com", "motorista4@umer.com", 
                           "motorista5@umer.com", "motorista6@umer.com", "motorista7@umer.com", "motorista8@umer.com", 
                           "motorista9@umer.com", "motorista10@umer.com", "cliente1@umer.com", "cliente2@umer.com", 
                           "cliente3@umer.com", "cliente4@umer.com", "cliente5@umer.com", "cliente6@umer.com", 
                           "cliente7@umer.com", "cliente8@umer.com", "cliente9@umer.com", "cliente10@umer.com", 
                           "cliente11@umer.com", "cliente12@umer.com", "cliente13@umer.com", "cliente14@umer.com", 
                           "cliente15@umer.com", "cliente16@umer.com","cliente17@umer.com", "cliente18@umer.com", 
                           "cliente19@umer.com", "cliente20@umer.com"};
                           
        String[] nomes = {"Motorista1", "Motorista2", "Motorista3", "Motorista4", "Motorista5", "Motorista6", "Motorista7", "Motorista8", 
                          "Motorista9", "Motorista10", "Cliente1", "Cliente2", "Cliente3", "Cliente4", "Cliente5", "Cliente6", "Cliente7", 
                          "Cliente8", "Cliente9", "Cliente10", "Cliente11", "Cliente12", "Cliente13", "Cliente 14", "Cliente 15", "Cliente 16",
                          "Cliente17", "Cliente18", "Cliente19", "Cliente20"};
        
        String[] passwords = {"motorista1", "motorista2", "motorista3", "motorista4", "motorista5", "motorista6", "motorista7", "motorista8", 
                              "motorista9", "motorista10", "cliente1", "cliente2", "cliente3", "cliente4", "cliente5", "cliente6", "cliente7", 
                              "cliente8", "cliente9", "cliente10", "cliente11", "cliente12", "cliente13", "cliente14", "cliente15", "cliente16",
                              "cliente17", "cliente18", "cliente19", "cliente20"};
        
        String[] moradas = {"Porto", "Lisboa", "Penafiel", "Marco de Canaveses", "Famalicão", "Santo Tirso", "Braga", "Travagem", 
                            "Ermesinde", "Vila Real", "Vila das Aves", "Vila das Aves", "Agualonga", "Reguenga", "Geres", "Régua", 
                            "Sesimbra", "Portalegre", "Castelo Branco", "Guarda", "Viseu", "Évora", "Beja", "Faro", "Leiria", "Aveiro",
                            "Bragança", "Lisboa", "Famaligato", "Braga"};
        
        String[] matriculas = {"01-AA-11", "02-BB-22", "03-CC-33", "04-DD-44", "05-EE-55", "06-FF-66", "07-GG-77", "08-HH-88", 
                               "09-II-99", "10-JJ-00"};
                            
        Random rand = new Random();
        HashMap<String,Actor> actores = umer.getActores();
        ArrayList<Motorista> motoristas = umer.getMotoristas();
        ArrayList<Taxi> taxis = umer.getTaxis();
        InfoReservas info = umer.getInfoReservas();
        
        int[] meses = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6};

        int[] anos = {1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1984, 1985, 1986, 
                      1991, 1992, 1993, 1994, 1985, 1986, 1986, 1987, 1990, 1991};
        int dia = 0;

        double factorRandom = 0 + (1 - 0) * rand.nextDouble();
        
        double velMedia = 60, precoKm = 1.5, totalFaturado = 0;
        for (int i = 0; i < 30; i++) {
            Calendar cal = new GregorianCalendar(anos[i], meses[i], dia + 1);
            Ponto2D p = new Ponto2D(0,0);
            ArrayList<Cliente> clientes = new ArrayList<Cliente>();
            ArrayList<Viagem> viagens = new ArrayList<Viagem>();
            Taxi t = new Taxi();
            if (i >= 0 && i < 8) {
                Motorista vazio = new Motorista(); 
                if (i == 0) {
                    Motorista m = new Motorista(emails[i], nomes[i], passwords[i], moradas[i], cal, 1, 0, 0, true, viagens, 0);
                    motoristas.add(m.clone());
                    Carros_Ligeiros aux = new Carros_Ligeiros(matriculas[i], m, velMedia, precoKm, factorRandom, totalFaturado, p, viagens);
                    taxis.add(aux.clone());
                    actores.put(emails[i], m.clone());
                }
                else if (i % 2 == 0) {
                    Motorista m = new Motorista(emails[i], nomes[i], passwords[i], moradas[i], cal, 1, 0, 0, true, viagens, 0);
                    motoristas.add(m.clone());
                    Carrinhas aux = new Carrinhas(matriculas[i], m, velMedia - 10, precoKm + 0.2, factorRandom, totalFaturado, p, viagens);
                    taxis.add(aux.clone());
                    actores.put(emails[i], m.clone());
                }
                else {
                    Motorista m = new Motorista(emails[i], nomes[i], passwords[i], moradas[i], cal, 1, 0, 0, true, viagens, 0);
                    motoristas.add(m.clone());
                    Motos aux = new Motos(matriculas[i], m, velMedia + 20, precoKm + 0.5, factorRandom, totalFaturado, p, viagens);
                    taxis.add(aux.clone());
                    actores.put(emails[i], m.clone());
                }
            }
            else if (i >= 8 && i < 10) {
                Motorista m = new Motorista(emails[i], nomes[i], passwords[i], moradas[i], cal, 1, 0, 0, true, viagens, 0);
                motoristas.add(m.clone());
                Carrinhas_comReservas aux = new Carrinhas_comReservas(matriculas[i], m, velMedia - 10, precoKm + 0.2, factorRandom, totalFaturado, p, viagens, clientes);
                taxis.add(aux.clone());
                actores.put(emails[i], m.clone());
            }
            else {
                Cliente c = new Cliente(emails[i], nomes[i], passwords[i], moradas[i], cal, p, t, viagens, false);
                actores.put(emails[i], c.clone());
            } 
        }
        
        umer.setActores(actores);
        umer.setMotoristas(motoristas);
        umer.setTaxis(taxis);
        umer.setInfoReservas(info);
    }
    */
}
