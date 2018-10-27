package data;

import business.Turno;
import business.UnidadeCurricular;
import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class UnidadeCurricularDAO {
    
    public static ArrayList<UnidadeCurricular> getAll() {
        ArrayList<UnidadeCurricular> uCurriculares = new ArrayList<UnidadeCurricular>();
        ArrayList<Turno> turnos = new ArrayList<Turno>();
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM UnidadeCurricular");
            ResultSet rs = stm.executeQuery();
            
            while (rs.next()) {
                String idUC = rs.getString("idUC");
                String nome = rs.getString("nome");
                int nAulasPrev = rs.getInt("nAulasPrevistas");
                UnidadeCurricular uc = new UnidadeCurricular(idUC, nome,nAulasPrev);
                uCurriculares.add(uc);
            }
            
            for (UnidadeCurricular uc : uCurriculares) {
                String id = uc.getidUC();
                PreparedStatement ps = conn.prepareStatement("SELECT * FROM Turno WHERE UC_idUC = ? and capacidadeAtual < capacidadeMax");
                ps.setString(1, id);
                ResultSet r = ps.executeQuery();
                
                while (r.next()) {
                    int idTurno = r.getInt("idTurno");
                    String tipo = r.getString("tipoTurno");
                    String sala = r.getString("sala");
                    int capacidadeAtual = r.getInt("capacidadeAtual");
                    int capacidadeMax = r.getInt("capacidadeMax");
                    
                    Turno aux = new Turno(idTurno, tipo, sala, capacidadeAtual, capacidadeMax, id);
                    turnos.add(aux);
                }
                
                uc.setTurnos(turnos);
                turnos.clear();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return uCurriculares;
    }
    
    public static ArrayList<UnidadeCurricular> getAllUCs (int idAluno) {
        ArrayList<UnidadeCurricular> uCurriculares = new ArrayList<UnidadeCurricular>();
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM UnidadeCurricular_Has_Aluno JOIN UnidadeCurricular ON UnidadeCurricular_idUC = idUC WHERE Aluno_idAluno = ?");
            stm.setInt(1, idAluno);
            ResultSet rs = stm.executeQuery();
            
            while (rs.next()) {
                String idUC = rs.getString("idUC");
                String nome = rs.getString("nome");
                int nAulas = rs.getInt("nAulasPrevistas");
                UnidadeCurricular uc = new UnidadeCurricular(idUC,nome,nAulas);
                uCurriculares.add(uc);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return uCurriculares;
    }
    
    public static void put (UnidadeCurricular ucs, int nTurnos) throws MySQLIntegrityConstraintViolationException {
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("INSERT INTO UnidadeCurricular\n" +
                "VALUES (?, ?, ?)\n");
            stm.setString(1, ucs.getidUC());
            stm.setString(2, ucs.getNome());
            stm.setInt(3,ucs.getnAulasPrevistas());
            stm.executeUpdate();
            
            //Inserir na tabela Turno o nº de turnos pretendidos pelo Docente.
            for (int i = 0; i < nTurnos; i++) {
                PreparedStatement stm2 = conn.prepareStatement("INSERT INTO Turno\n" +
                    "(tipoTurno, sala, capacidadeAtual, capacidadeMax, UC_idUC) VALUES (?, ?, ?, ?, ?)\n");
                stm2.setString(1, "TP" + (i + 1));
                stm2.setString(2, null);
                stm2.setInt(3, 0);
                stm2.setInt(4, 30);
                stm2.setString(5, ucs.getidUC());
                stm2.executeUpdate();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    public static int getFaltasUC(String idUC){
        Connection conn = null;
        int faltas = -1;
        try {
            conn = Connect.connect();
                               
            PreparedStatement st = conn.prepareStatement("SELECT nAulasPrevistas\n" +
                                                         "FROM UnidadeCurricular WHERE idUC = ?");
            st.setString(1, idUC);
            ResultSet rs = st.executeQuery();
            
            if (rs.next()) {
                faltas = (int) (rs.getInt("nAulasPrevistas")*0.25);
            }           
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return faltas;
    }
}
