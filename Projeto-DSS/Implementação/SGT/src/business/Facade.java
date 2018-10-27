package business;

import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException;
import data.AlunoDAO;
import data.DocenteDAO;
import data.TrocaDAO;
import data.TurnoDAO;
import data.UnidadeCurricularDAO;
import java.util.ArrayList;
import java.util.Map;

/**
 * Facade => Classe que trata de manusear e tratar os dados da aplicação.
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Facade {
    /* Fácil acesso aos dados do utilizador que está em sessão na aplicação. */
    private Utilizador sessao;
    
    /**
     * Construtor de uma nova Facade.
     */
    public Facade() {
        this.sessao = new Utilizador();
    }
    
    /**
     * Método getSessao()
     * 
     * @return Utilizador
     */
    public Utilizador getSessao() {
        return this.sessao;
    }
    
    /**
     * Método setSessao()
     * 
     * @param user 
     */
    public void setSessao (Utilizador user) {
        this.sessao = user;
    }
    
    
    /* ********************************************* *
     * Conjunto de métodos chamados pelo LoginFrame. *
     * ********************************************* */
    
    /**
     * Método que verifica o login do utilizador.
     * Neste método é verificado o input do utilizador, verificando assim
     * se ele faz parte ou não do sistema.
     * Aqui é chamado o AlunoDAO e o DocenteDAO, de forma a obter e confirmar
     * a informação do input com a do sistema. 
     * 
     * É, por fim, retornada uma flag, tendo esta 3 valores possíveis:
     * -> flag = 0 (Significa que foi um Aluno que se autenticou);
     * -> flag = 1 (Significa que foi um Docente que se autenticou);
     * -> flag = -1 (Significa que o input dado pelo utilizador está incorreto).
     * 
     * @param user
     * @param password
     * @return
     * @throws DadosInvalidosException
     * @throws UtilizadorInexistenteException 
     */
    public int login (String user, String password) throws DadosInvalidosException, 
                                                           UtilizadorInexistenteException 
    {
        int flag = -1;

        if (user.charAt(0) == 'a') { //Verificar se é aluno.
            int idAluno = Integer.parseInt(user.replaceAll("[^0-9]", ""));
            try {
                Aluno a = AlunoDAO.get(idAluno);
                flag = a.login(user, password);
                this.sessao = a;
            } catch (UtilizadorInexistenteException | DadosInvalidosException e) {
                flag = -1;
            }
        } else if (user.charAt(0) == 'd') { //Verifica se é docente.
            int idDocente = Integer.parseInt(user.replaceAll("[^0-9]", ""));
            try {
                Docente d = DocenteDAO.get(idDocente);
                flag = d.login(user, password);
                this.sessao = d;
            } catch (UtilizadorInexistenteException e) {
                flag = -1;
            }
        }
        
        return flag;
    }
    
    /**
     * Método que regista um Aluno no sistema.
     * Aqui é invocado o método registaAluno() contido na classe Aluno.
     * Esse comunicará com o respetivo DAO, registando assim o Aluno no sistema.
     * 
     * @param user
     * @param password
     * @param nome
     * @param numero
     * @param estatuto
     * @throws DadosInvalidosException 
     */
    public void registaUser (String user, String password, String nome, 
                             int numero, String estatuto) throws DadosInvalidosException 
    {
        Aluno a = new Aluno(user, password, nome, numero, estatuto);
        a.registaAluno();
    }
    
    
    /* ********************************************* *
     * Conjunto de métodos chamados pelo MenuAluno.  *
     * ********************************************* */
    
    /**
     * Método que devolve todas as UC's de um determinado Aluno.
     * Este método determina qual o Aluno em sessão, e comunica com 
     * a UnidadeCurricularDAO, retornando assim uma coleção de todas as UC's
     * onde este aluno está registado.
     * 
     * @return
     */
    public ArrayList<UnidadeCurricular> getAllUCs() {
        Aluno a = (Aluno)this.sessao;
        return UnidadeCurricularDAO.getAllUCs(a.getNumero());
    }
    
    /**
     * Método que devolve todas as UC's que estão registadas no sistema.
     * É devolvida uma coleção de todas as UC's no sistema, comunicando com
     * a classe UnidadeCurricularDAO.
     * 
     * @return 
     */
    public ArrayList<UnidadeCurricular> getAll() {
        return UnidadeCurricularDAO.getAll();
    }
    
    /**
     * Método que remove o registo de uma UC de um determinado Aluno.
     * É comunicado ao AlunoDAO de que o Aluno em sessão removeu uma determinada
     * UC dos seus registos.
     * 
     * @param idUCs
     * @param tipoTurno 
     */
    public void unsubscribeUC (String idUCs, String tipoTurno) {
        Aluno a = (Aluno)this.sessao;
        AlunoDAO.unsubscribeUC(idUCs, a.getNumero(), tipoTurno);
    }
    
    /**
     * Método que regista uma nova UC para um determinado Aluno.
     * Este método comunica com o AlunoDAO de forma a registar no sistema a
     * nova UC do Aluno. 
     * Aqui é também "apanhada" uma exceção no caso do o Alun já estiver 
     * inscrito nessa UC.
     *
     * @param idUCs
     * @param tipoTurno
     * @return 
     */
    public boolean subscribeUC (String idUCs, String tipoTurno) {
        boolean flag = true;
        Aluno a = (Aluno)this.sessao;
        try {
            AlunoDAO.registaUC(idUCs, a.getNumero(), tipoTurno);
        } catch (MySQLIntegrityConstraintViolationException e) {
            flag = false;
        }
        
        return flag;
    }
    
    /**
     * Método que completa a troca aceite por um Aluno.
     * Aqui é finalizada a Troca que anteriormente foi registada por outro Aluno.
     * É comunicado ao TrocaDAO toda a informação necessária para assim serem
     * feitas todas as alterações necessárias.
     * É também atualizado o Map<idUC, tipoTurno> contido no Aluno.
     * 
     * @param idAlunoReq
     * @param idUC
     * @param tipoTurnoPretendido 
     */
    public void resolverTrocaAluno (int idAlunoReq, String idUC, 
                                    String tipoTurnoPretendido) 
    {
        Aluno a = (Aluno)this.sessao;
        int idAlunoAceita = a.getNumero();
        String tipoTurnoAtual = a.getTurnos().get(idUC);
        
        TrocaDAO.aceitaTrocaCompleta(idUC, idAlunoReq, idAlunoAceita, 
                                     tipoTurnoAtual, tipoTurnoPretendido);
        
        /* Atualizar Map<idUC, tipoTurno> turnos do Aluno. */
        Map<String, String> turnos = a.getTurnos();
        turnos.put(idUC, tipoTurnoPretendido);
        a.setTurnos(turnos);
    }
    
    /**
     * Método que verifica troca feita por um Aluno.
     * Aqui é comunidaco ao TrocaDAO que foi efetuado a tentativa de um registo
     * de uma nova troca.
     * É também confirmado o estatuto do Aluno:
     * -> Se este for simplesmente "Estudante", então a troca é introduzida no sistema.
     * -> Senão, tentamos inserir o Aluno diretamente no turno, caso existe capacidade suficiente.
     * 
     * @param idUC
     * @param turnoAtual
     * @param turnoPretendido
     * @return 
     */
    public int resolveTrocaAluno (String idUC, String turnoAtual, 
                                  String turnoPretendido) 
    {
        int flag = 0;
        Aluno a = (Aluno)this.sessao;
        int idAluno = a.getNumero();
        
        if (a.getEstatuto().equals("Estudante")) {
            System.out.println("Sou estudante");
            TrocaDAO.put(new Troca(turnoAtual, turnoPretendido, idAluno, -1, idUC));
        } else {
            try {
                TurnoDAO.insereAlunoEstatuto(idAluno, idUC, turnoPretendido);
                flag = 1;
            } catch (TurnoCheioException e) {
                flag = -1;
            }
        }
        
        return flag;
    }
    
    /**
     * Método que retorna uma coleção dos turnos de uma determinada UC.
     * 
     * @param idUC
     * @return 
     */
    public ArrayList<String> getTurnosUC (String idUC) {
        return TurnoDAO.getTurnosUC(idUC);
    }
    
    /**
     * Método que determina as Trocas disponíveis de uma UC.
     * Este método devolve uma coleção de todas as trocas disponíveis de 
     * uma determinada UC. Essa coleção foi previamente filtrada para apenas ter
     * as UC's as quais o Aluno em sessão NÃO está.
     * 
     * @param turnoAtual
     * @param idUC
     * @return 
     */
    public ArrayList<Troca> getTrocasUC (String turnoAtual, String idUC) {
        ArrayList<Troca> trocas = (ArrayList)TrocaDAO.get(idUC);

        /* Filtra só aquelas trocas cujo turno pretendido é igual ao turno
           do Aluno em sessão. */
        trocas.removeIf(t -> !t.getTurnoPretendido().equals(turnoAtual));
        
        return trocas;
    }
    
    
    /* ********************************************* *
     * Conjunto de métodos chamados pelo MenuDocente.*
     * ********************************************* */
    
    /**
     * Método que altera a capacidade de um turno.
     * Este método comunica com o TurnoDAO para alterar a capacidade de um
     * respetivo turno.
     * 
     * @param tipoTurno
     * @param newCapacidad 
     */
    public void alteraCapacidade (String tipoTurno, Integer newCapacidad) {
        Docente d = (Docente)this.sessao;
        String idUC = d.getidUC();
        TurnoDAO.alteraCpcd(idUC, tipoTurno ,newCapacidad);
    }
    
    /**
     * Método que regista uma nova UC no sistema.
     * Aqui é chamada a classe UnidadeCurricular, onde esta irá tratar de 
     * chamar o respetivo DAO para registar a UC no sistema.
     * 
     * @param idUC
     * @param nomeUC
     * @param nTurnos
     * @param nAulasPrevistas
     * @return 
     */
    public boolean registaUC (String idUC, String nomeUC, int nTurnos, int nAulasPrevistas) {
        boolean flag;
        UnidadeCurricular uc = new UnidadeCurricular(idUC, nomeUC, nAulasPrevistas);
        
        flag = uc.registaUC(nTurnos);
        
        return flag;
    }
    
    /**
     * Método que adiciona uma falta ao sistema.
     * É comunicado ao TurnoDAO para adicionar o falta a um determinado Aluno.
     * 
     * @param idAluno
     * @param idUC
     * @param tipoTurno
     * @param falta 
     */
    public void adicionafalta (int idAluno, String idUC, 
                               String tipoTurno, int falta) 
    {
        TurnoDAO.addfalta(idAluno, idUC, tipoTurno, falta);
    }
    
    /**
     * Método que determina as faltas de todos os alunos de um turno.
     * Este método devolve um Map<Integer, String[]>, o qual contém, 
     * para cada Integer correspondente ao número de identificação do Aluno, 
     * um array de String[2], o qual contém na primeira posição o nome do Aluno,
     * seguido do número de faltas (em String) desse aluno ao respetivo turno.
     * 
     * 
     * @param tipoTurno
     * @param idUC
     * @return 
     */
    public Map<Integer, String[]> getFaltasAlunos (String tipoTurno, String idUC) {
        return TurnoDAO.getFaltasAlunos(tipoTurno, idUC);
    }
    
    /**
     * Método que determina a capacidade de um turno de uma determinada UC.
     * 
     * @param idUC
     * @param tipoTurno
     * @return 
     */
    public int getCapTurno (String idUC, String tipoTurno) {
        int cap = TurnoDAO.getidTurno_capTurno(idUC, tipoTurno)[1];
        
        return cap;
    }
    
    /**
     * Método que efetua uma troca da parte do Docente.
     * Aqui é chamada a classe Docente, a qual se irá encarregar de comunicar
     * ao respetivo DAO, o qual irá fazer uma troca entre Alunos.
     * 
     * @param idAluno
     * @param tipoTurno
     * @throws CapMaxeNenhumComFaltasException 
     */
    public void trocaAlunoDocente (int idAluno, String tipoTurno) throws CapMaxeNenhumComFaltasException{
        Docente d = (Docente)this.sessao;
        d.efetuaTrocaD(d.getidUC(),idAluno,tipoTurno);
    }


    
    

    

}
