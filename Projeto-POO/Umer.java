
/**
 * Esta classe é a responsável pelo manuseamento de todos os dados da aplicação.
 * É aqui que são guardadas as estruturas de dados respetivas a todos os actores
 * do sistema, bem como todas as viaturas. 
 * 
 * Para guardar estes actores, foi usado um HashMap<String, Actor> de forma a
 * ter fácil acesso a qualquer actor no sistema através do seu email, sendo este
 * a chave do HashMap.
 *
 * Para o manuseamento das viaturas e motoristas do sistema, utilizamos
 * simplesmente um ArrayList<Taxi> e ArrayList<Motorista>, visto que era uma
 * forma prática de guardar os objetos.
 *
 * Ainda existe uma variável com o tipo InfoReservas, sendo esta a que guarda a
 * informação de uma reserva feita por um Cliente. A classe InfoReservas foi
 * criada com o intuíto de auxiliar o tratamento do problema das filas de espera. 
 * 
 * @author David Sousa
 * @author Carlos Pedrosa
 * @author Manuel Sousa
 */

import java.io.*;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.Calendar;
import java.util.GregorianCalendar;

public class Umer implements Serializable {
    /** Variáveis de Instância */
    private HashMap<String,Actor> actores;
    private ArrayList<Taxi> taxis;
    private ArrayList<Motorista> motoristas;
    private InfoReservas info;
    
    /** 
     * Construtor Vazio.
     * Cria uma instância de Umer.
     */
    public Umer() {
        this.actores = new HashMap<String,Actor>();
        this.taxis = new ArrayList<Taxi>();
        this.motoristas = new ArrayList<Motorista>();
        this.info = new InfoReservas();
    }
    
    /** 
     * Construtor por cópia.
     * @param bd
     */
    public Umer(Umer bd) {
        this.actores = bd.getActores();
        this.taxis = bd.getTaxis();
        this.motoristas = bd.getMotoristas();
        this.info = bd.getInfoReservas();
    }
    
    /**
     * Construtor por parametro.
     * @param actores
     * @param taxis
     * @param motoristas
     * @param info
     */
    public Umer(HashMap<String,Actor> actores, ArrayList<Taxi> taxis, ArrayList<Motorista> motoristas, InfoReservas info) {
        for (Actor a: actores.values()) {
            this.actores.put(a.getEmail(), a.clone());
        }
        
        for (Taxi t: taxis) {
            this.taxis.add(t.clone());
        }
        
        for (Motorista m: motoristas) {
            this.motoristas.add(m.clone());
        }
        this.info = info.clone();
    }
    
    /**
     * Conjunto de Getters e Setters da classe.
     */
    public HashMap<String,Actor> getActores() {
        HashMap <String,Actor> copiaAtor = new HashMap<>();
        
        for (Actor a: this.actores.values()) {
            copiaAtor.put(a.getEmail(), a.clone());
        }
        
        return copiaAtor;
    }

    public void setActores (HashMap<String,Actor> actores) {
        this.actores.clear();
        
        for (Actor a : actores.values()) {
            this.actores.put(a.getEmail(), a.clone());
        }
    }
    
    public ArrayList<Taxi> getTaxis() {
        ArrayList<Taxi> copiaTaxi = new ArrayList<Taxi>();
        
        for (Taxi t : taxis) {
            copiaTaxi.add(t.clone());
        }

        return copiaTaxi;
    }

    public void setTaxis (ArrayList<Taxi> taxis) {
        this.taxis.clear();
       
        for (Taxi t : taxis) {
            this.taxis.add(t.clone());
        }
    }
    
    public ArrayList<Motorista> getMotoristas() {
        ArrayList<Motorista> copiaMot = new ArrayList<Motorista>();
        
        for (Motorista m : motoristas) {
            copiaMot.add(m.clone());
        }

        return copiaMot;
    }

    public void setMotoristas (ArrayList<Motorista> motoristas) {
        this.motoristas.clear();

        for (Motorista m : motoristas) {
            this.motoristas.add(m.clone());
        }
    }
    
    public InfoReservas getInfoReservas() {
        return this.info.clone();
    }
    
    public void setInfoReservas (InfoReservas info) {
        this.info = info.clone();
    }
    
    /**
     * Obter um motoristas através do seu email.
     * @param email
     * @return
     */
    public Motorista getMotoristaByEmail (String email) {
        Motorista aux = new Motorista();
        
        for (Taxi t : this.taxis) {
            if (t.getMotorista().getEmail().equals(email)) {
                aux = t.getMotorista(); 
                return aux;
            }
        }
        return aux;
    }
    
    /**
     * Obter um Táxi através do seu email.
     * @param email
     * @return
     */
    public Taxi getTaxiByEmail (String email) {
        Taxi aux = new Taxi();
        
        for (Taxi t : this.taxis) {
            if (t.getMotorista().getEmail().equals(email)) {
                aux = t.clone(); 
                return aux;
            }
        }
        return aux;
    }
    
    /** 
     * Obter um Táxi através da sua matrícula.
     * @param matricula
     * @return
     */
    public Taxi getTaxiByMatricula(String matricula) {
        Taxi aux = new Taxi();
        
        for (Taxi t: this.taxis) {
            if (t.getMatricula().equals(matricula)) {
                aux = t.clone();
                return aux;
            }
        }
        return aux;
    }
    
    /**
     * Obter a Lista de Reservas de um certo Motorista do sistema.
     * @param m
     * @return
     */
    public ArrayList<Cliente> getReservasByMotorista (Motorista m) {
        ArrayList<Cliente> reservas = new ArrayList<Cliente>();
        ArrayList<Cliente> newRevs = new ArrayList<Cliente>();
        
        for (Taxi t: this.taxis) {
            Motorista aux = t.getMotorista();
            if (m.equals(aux)) {
                if (t instanceof Carros_Ligeiros_comReservas) {
                    Carros_Ligeiros_comReservas rev = (Carros_Ligeiros_comReservas) t;
                    reservas = rev.getReservas();
                }
                else if (t instanceof Carros_Ligeiros_comReservas) {
                    Carrinhas_comReservas rev = (Carrinhas_comReservas) t;
                    reservas = rev.getReservas();
                }
                else if (t instanceof Motos_comReservas) {
                    Motos_comReservas rev = (Motos_comReservas) t;
                    reservas = rev.getReservas();
                }
                return reservas;
            }
        }
        
        return reservas;
    }
    
    /** 
     * Obter uma Lista com os emails de todos os Motoristas do sistema.
     * @return
     */
    public ArrayList<String> getEmailsMotoristas() {
        ArrayList<String> emails = new ArrayList<String>();
        
        for (Taxi t: this.taxis) {
            Motorista aux = t.getMotorista();
            if (aux.getDisponivel()) emails.add(aux.getEmail());
        }
        
        return emails;
    }
    
    /** 
     * Obter uma Lista com as matricula de todos os Táxis do sistema.
     * @return
     */
    public ArrayList<String> getMatriculasTaxis() {
        ArrayList<String> matriculas = new ArrayList<String>();
        
        for (Taxi t : this.taxis) {
            matriculas.add(t.getMatricula());
        }
        
        return matriculas;
    }
    
    /** 
     * Introduz um Táxi vazio (Taxi()) num certo Motorista.
     * @param m
     */
    public void setTaxiVazio (Motorista m) {
        Taxi aux = new Taxi();
        
        for (Taxi t : this.taxis) {
            if (t.getMotorista().equals(m)) {
                t = aux;
            }
        }
    }
    
    /* Métodos para Leitura e Escrita em Ficheiros */
    public static Umer startApp() throws FileNotFoundException, IOException, ClassNotFoundException {
        ObjectInputStream ois = new ObjectInputStream (new FileInputStream("estado.txt"));
        Umer u = (Umer) ois.readObject();
        ois.close();
        return u;
    }

    public void saveApp (String nomeFicheiro) throws FileNotFoundException, IOException {
        ObjectOutputStream oos = new ObjectOutputStream (new FileOutputStream(nomeFicheiro));
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    public void log (String f, boolean ap) throws IOException {
        FileWriter fw = new FileWriter(f, ap);
        fw.write("\n---- LOG - LOG ---- \n");
        fw.write(this.toString());
        fw.write("\n---- LOG - LOG ---- \n");
        fw.flush();
        fw.close();
    }
    /* ------------------------------------------------------------------------------------------ */

    /** 
     * Cria um novo utilizador no sistema;
     * Se esse utilizador já existir, envia uma excepção.
     * @param a
     */
    public void criarUtilizador(Actor a) throws ActorRepetidoException {
        if (actores.containsKey(a.getEmail())) throw new ActorRepetidoException("Utilizador já existente!");
        
        if (a instanceof Motorista) {
            Motorista m = (Motorista) a;
            motoristas.add(m.clone());
        }
        
        actores.put(a.getEmail(), a.clone());
    }
    
    /** 
     * Calcula a distância entre dois Pontos no plano.
     * @param inicio
     * @param fim
     * @return
     */
    public double calculaDistanciaViagem (Ponto2D inicio, Ponto2D fim) {
        return (Math.sqrt(Math.pow(fim.getX() - inicio.getX(), 2) + Math.pow(fim.getY() - inicio.getY(), 2)));
    }
    
    /** 
     * Calcula o tempo de uma viagem, consoante as coordenadas e velocidade
     * média do Táxi.
     * @param inicio
     * @param fim
     * @param t
     * @return
     */
    public double calculaTempoViagem (Ponto2D inicio, Ponto2D fim, Taxi t) {
        double distViagem = calculaDistanciaViagem(inicio, fim);
        double tempoViagem = (distViagem)/(t.getVelMedia());  

        return tempoViagem;
    }
    
    /**
     * Calcula o custo estimado da viagem, consoante a distância da viagem e o
     * preço por Km do Táxi.
     * @param inicio
     * @param fim
     * @param t
     * @return
     */
    public double calculaCustoEstimadoViagem (Ponto2D inicio, Ponto2D fim, Taxi t) {
        double distViagem = calculaDistanciaViagem(inicio, fim);
        double precoViagem = distViagem * (t.getPrecoKm());
        
        return precoViagem;
    }
    
    /** 
     * Calcula qual o Táxi mais próximo de uma determinada coordenada;
     * Envia ainda uma excepção se não existirem táxis disponíveis.
     * @param inicio
     * @return
     */
    public Taxi selecionaTaxiDisponivelMaisProximo (Ponto2D inicio) throws ArrayListVazioException {
        double distProx, distNew;
        Taxi maisProximo = new Taxi();
        ArrayList<Taxi> taxisDisponiveis = new ArrayList<Taxi>();
        
        if (this.taxis == null) throw new ArrayListVazioException("Não existem Taxis!");
        
        //Copia os taxistas disponiveis para outro ArrayList
        for (Taxi t: this.taxis) {
            Motorista aux = t.getMotorista();
            if (aux.getDisponivel()) taxisDisponiveis.add(t);
        }
        
        maisProximo = taxisDisponiveis.get(0);
        distProx = calculaDistanciaViagem(inicio, maisProximo.getCoords());
        for (Taxi t: taxisDisponiveis) {
            distNew = calculaDistanciaViagem(inicio, maisProximo.getCoords());
            if (distNew < distProx) maisProximo = t.clone();
        }
        
        return maisProximo;
    }
    
    /**
     * Verifica se um determinado Cliente do sistema está contido em alguma
     * Lista de Reservas de algum dos Táxis do sistema.
     * @param c
     * @return
     */
    public boolean existeEmReservas (Cliente c) {
        ArrayList<Cliente> reservasAux = new ArrayList<Cliente>();
        for (Taxi t : this.taxis) {
            if (t instanceof Carros_Ligeiros_comReservas) {
                Carros_Ligeiros_comReservas aux = (Carros_Ligeiros_comReservas) t;
                reservasAux = aux.getReservas();
            }
            if (t instanceof Carrinhas_comReservas) {          
                Carrinhas_comReservas aux = (Carrinhas_comReservas) t;
                reservasAux = aux.getReservas();
            }    
            if (t instanceof Motos_comReservas) {
                Motos_comReservas aux = (Motos_comReservas) t;
                reservasAux = aux.getReservas();
            }

            for (Cliente cli : reservasAux) {
                if (cli.getEmail().equals(c.getEmail())) return true;
            }

            reservasAux.clear();
        }
        return false;
    }
    
    /** 
     * Efetua a viagem que foi reservada por um determinado Cliente;
     * A viagem é efetuada quando a Lista de Reservas em causa tem mais do que 1
     * elemento.
     * @param info
     * @param newReservas
     */
    public void fazerViagemReservada (InfoReservas info, ArrayList<Cliente> newReservas) {
        Taxi t = info.getTaxi();
        Cliente c = info.getCliente();
        Ponto2D coordsDestinoCliente = info.getCoords();
        boolean taxiProximo = info.getBoolProx();
        int indexTaxi = info.getIndexTaxi();
        int indexMotorista = info.getIndexMotorista();
        
        double distTaxi_Cliente = Math.round(calculaDistanciaViagem(t.getCoords(), c.getCoords()) * 100.0)/100.0;
        double tempoTaxi_Cliente = calculaTempoViagem(t.getCoords(), c.getCoords(), t);

        double distCliente_Destino = Math.round(calculaDistanciaViagem(c.getCoords(), coordsDestinoCliente) * 100.0)/100.0; 
        double tempoCliente_Destino = calculaTempoViagem(c.getCoords(), coordsDestinoCliente, t);
        
        double custoEstimadoViagem = Math.round((calculaCustoEstimadoViagem(c.getCoords(), coordsDestinoCliente, t)
                                     + calculaCustoEstimadoViagem(t.getCoords(), c.getCoords(), t)) * 100.0)/100.0;
        double custoRealViagem = custoEstimadoViagem;
                                     
        double tempoEstimado = tempoTaxi_Cliente + tempoCliente_Destino;   
        double tempoReal = tempoEstimado + (t.getFactor() * tempoEstimado);
        double difTempos = tempoReal - tempoEstimado;
        
        if (difTempos <= (0.25 * tempoEstimado)) {
            custoRealViagem = Math.round((custoRealViagem + (t.getPrecoKm() * (difTempos * t.getVelMedia()))) * 100.0)/100.0;
        }
        
        ArrayList<Viagem> viagens = new ArrayList<Viagem>();
        Motorista m = t.getMotorista();
        double cumpHorario = (tempoReal * 100)/tempoEstimado; 
        
        m.setKmsRealizados(distTaxi_Cliente + distCliente_Destino);
        m.setNViagensEfetuadas(m.getNViagensEfetuadas() + 1);
        m.setCumpHorario(cumpHorario);
        
        GregorianCalendar data = new GregorianCalendar();
        
        Viagem trip = new Viagem(m, c.getCoords(), coordsDestinoCliente.clone(), 
                                 taxiProximo, t.clone(), distTaxi_Cliente, distCliente_Destino, 
                                 custoEstimadoViagem, custoRealViagem, -1, data);
        viagens = c.getViagensEfetuadas();
        viagens.add(trip);
            
        c.setCoords(coordsDestinoCliente);
        c.setViagensEfetuadas(viagens);
            
        m.setViagensEfetuadas(viagens);
        m.setDisponivel(true);
        
        t.setMotorista(m); //Poe o motorista devolta no Taxi correspondente, depois de todas as alteraçoes.
        t.setCoords(coordsDestinoCliente);
        t.aumentarTotalFaturado(custoRealViagem);

        ArrayList<Viagem> auxViagensTaxi = t.getViagensEfetuadas();
        auxViagensTaxi.add(trip.clone());
        t.setViagensEfetuadas(auxViagensTaxi);
        
        ArrayList<Taxi> auxTaxis = getTaxis(); //O set para o ArrayList de taxis da classe Umer e feito dentro do if-statement de forma a usar o cast.
        if (t instanceof Carros_Ligeiros_comReservas) {
            Carros_Ligeiros_comReservas aux = (Carros_Ligeiros_comReservas) t;
            newReservas.remove(0); 
            aux.setReservas(newReservas);
            
            auxTaxis.set(indexTaxi, (Taxi) aux);
            setTaxis(auxTaxis);
        }
        else if (t instanceof Carrinhas_comReservas) {
            Carrinhas_comReservas aux = (Carrinhas_comReservas) t;
            newReservas.remove(0); 
            aux.setReservas(newReservas);
            
            auxTaxis.set(indexTaxi, (Taxi) aux);
            setTaxis(auxTaxis);
        }
        else if (t instanceof Motos_comReservas) {
            Motos_comReservas aux = (Motos_comReservas) t;            
            newReservas.remove(0); 
            aux.setReservas(newReservas);
            
            auxTaxis.set(indexTaxi, (Taxi) aux);
            setTaxis(auxTaxis);
        } 
        
        HashMap<String,Actor> auxActores = getActores();
        auxActores.replace(c.getEmail(), c);
        auxActores.replace(m.getEmail(), m);
        setActores(auxActores);

        ArrayList<Motorista> auxMotoristas = getMotoristas();
        auxMotoristas.set(indexMotorista, m); //Substitui o Motorista m antigo pelo novo com as alteraçoes feitas.
        setMotoristas(auxMotoristas);
    }
    
    /** 
     * Verifica se existe um determinado email nos diferentes actores do
     * sistema.
     * @param email
     * @return
     */
    public boolean existeEmail(String email) {
        return actores.containsKey(email);
    }
    
    /** 
     * Seleciona um Actor de uma Lista de Actores;
     * Este método é auxiliar de dois métodos, "top5Desvios" e "top10Clientes";
     * Através da flag, verifica para qual método está a fazer o trabalho:
     * Se for 0, procura o Actor com menos despesa para o "top10Clientes";
     * Se for 1, procura o Actor com menos desvios para o "top5Desvios";
     * @param actores
     * @param flag
     * @return
     */
    public Actor menorActorLista(ArrayList<Actor> actores, int flag) {
        
        Actor a = new Actor();

        if (flag == 0) {
            a = actores.get(0).clone();
            Cliente c = (Cliente) a;
            double menorDespesa = c.despesaTotal();
            
            for (Actor act : actores) {
                Cliente aux = (Cliente) act;
                if (aux.despesaTotal() < menorDespesa) {
                    a = act.clone();
                    menorDespesa = aux.despesaTotal();
                }
            }
        }
        
        else { 
            a = actores.get(0).clone();
            Motorista m = (Motorista) a;
            double menoresDesvios = m.contaDesvios();
           
            for (Actor act : actores) {
                Motorista aux = (Motorista) act;
                double contaDesviosAux = aux.contaDesvios();
                
                if (contaDesviosAux < menoresDesvios) {
                    a = act.clone();
                    menoresDesvios = contaDesviosAux;
                }   
            }   
        }
        
        return a;
    }
    
    /** 
     * Seleciona os 10 Clientes com maior gasto no sistema.
     * @return
     */
    public ArrayList<Actor> top10Clientes() {
        int i;
        ArrayList<Actor> clientes = new ArrayList<Actor>();
        ArrayList<Actor> clientesAll = new ArrayList<Actor>();
        
        //Efetuar uma cópia de todos os Clientes na lista de Actores do sistema.
        for (Actor a : this.actores.values()) {
            if (a instanceof Cliente) {
                clientesAll.add(a.clone());
            }
        }

        for (i = 0; i < clientesAll.size(); i++) {
            if (i > 10) break;
            clientes.add(clientesAll.get(i));          
        }

        if (i == 10) {
            for (i = 10; i < clientesAll.size(); i++) {
                Cliente auxOld = (Cliente) clientes.get(i);
                double despesaTotalAux_Old = auxOld.despesaTotal();
                
                Cliente auxNew = (Cliente) menorActorLista(clientes, 0);
                double despesaTotalAux_New = auxNew.despesaTotal();
                
                if (despesaTotalAux_Old > despesaTotalAux_New) {
                    int index = clientes.indexOf((Actor) auxOld);
                    clientes.set(index, (Actor) auxNew.clone());
                }
            }
        }

        return clientes;
    } 
    
    /** 
     * Seleciona os 5 Motoristas com os maiores desvios no sistema.
     * @return
     */
    public ArrayList<Actor> top5Desvios() {
        int i;
        ArrayList<Actor> motoristas = new ArrayList<Actor>();
        ArrayList<Actor> motoristasAll = new ArrayList<Actor>();
        
        //Efetuar uma cópia de todos os Motoristas na lista de Actores do sistema.
        for (Actor a : this.actores.values()) {
            if (a instanceof Motorista) {
                motoristasAll.add(a.clone());
            }
        }

        for (i = 0; i < motoristasAll.size(); i++) {
            if (i > 10) break;
            motoristas.add(motoristasAll.get(i));
        }
        
        if (i == 10) {
            for (i = 10; i < motoristasAll.size(); i++) {
                Motorista auxOld = (Motorista) motoristas.get(i);
                double contaDesvios_Old = auxOld.contaDesvios();

                Motorista auxNew = (Motorista) menorActorLista(motoristas, 1);
                double contaDesvios_New = auxNew.contaDesvios();

                if (contaDesvios_Old > contaDesvios_New) {
                    int index = motoristas.indexOf((Actor) auxOld);
                    motoristas.set(index, (Actor) auxNew.clone());
                }
            }
        }
        
        return motoristas;
    }
    
    /** 
     * Verifica os dados de um determinado utilizador;
     * Se esse utilizador não existir ou a password não for a correspondente, é
     * enviada uma excepção.
     * @param email
     * @param password
     */
    public void verificaDados (String email, String password) throws ErroLogException {
        Actor a = actores.get(email);
        
        if (a == null) throw new ErroLogException("Utilizador não encontrado!");
        if (!a.getPassword().equals(password)) throw new ErroLogException("Password não correspondente!");
    }
    
    /** 
     * Devolve uma representação do sistema em modo textual.
     * @return
     */
    public String toString() {
        String aux = new String("====> ACTORES <====\n");
        for (Actor a : this.actores.values()) aux = aux.concat("\n" + a.toString() + "\n");
        
        aux = aux.concat("====> TAXIS <====\n");
        for (Taxi t : this.taxis) aux = aux.concat("\n" + t.toString() + "\n");
        
        aux = aux.concat("====> MOTORISTAS <====\n");
        for (Motorista m : this.motoristas) aux = aux.concat("\n" + m.toString() + "\n");
        
        return aux;
    }
}
