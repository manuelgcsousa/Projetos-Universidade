package data;

import business.UtilizadorInexistenteException;
import business.Aluno;
import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

public class AlunoDAO {

    public static Aluno get (int idAluno) throws UtilizadorInexistenteException {
        Aluno al = null;
        Connection conn = null;
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM Aluno WHERE idAluno=?");
            stm.setInt(1, idAluno);
            ResultSet rs = stm.executeQuery();
            if (rs.next()) {
                String user = rs.getString("username");
                String pass = rs.getString("password");
                String nome = rs.getString("nome");
                String estatuto = rs.getString("estatuto");
                al = new Aluno(user, pass, nome, idAluno, estatuto);
            } else {
                throw new UtilizadorInexistenteException("Utilizador Inexistente!");
            }
            
            Map<String, String> turnos = new HashMap<String, String>();
            
            PreparedStatement st = conn.prepareStatement("SELECT * FROM Aluno_has_Turno\n" +
                                                         "JOIN Turno ON Turno_idTurno = idTurno\n" + 
                                                         "WHERE Aluno_idAluno=?");
            st.setInt(1, idAluno);
            ResultSet r = st.executeQuery();
            while (r.next()) {
                String idUC = r.getString("UC_idUC");
                String tipoTurno = r.getString("tipoTurno");
                turnos.put(idUC, tipoTurno);
            }
            al.setTurnos(turnos);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return al;
    } 
    
    public static void put (int idAluno, Aluno value) {
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("INSERT INTO Aluno\n" +
                "VALUES (?, ?, ?, ?, ?)\n");

            stm.setInt(1, value.getNumero());
            stm.setString(2, value.getUsername());
            stm.setString(3, value.getPassword());
            stm.setString(4, value.getNome());
            stm.setString(5, value.getEstatuto());
            stm.executeUpdate();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    public static void registaUC (String idUC, int idAluno, String tipoTurno) 
            throws MySQLIntegrityConstraintViolationException {
        Connection conn = null;
        int idTurno = -1;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("INSERT INTO UnidadeCurricular_has_Aluno\n" 
                                                          + "VALUES (?, ?)\n");
            stm.setString(1, idUC);
            stm.setInt(2, idAluno);
            stm.executeUpdate();
            
            /* Através da UnidadeCurricular, determinar o turno respetivo 
               de forma a popular a tabela Aluno_has_Turno. */
            PreparedStatement stm2 = conn.prepareStatement("SELECT idTurno\n"
                                                           + "FROM Turno\n"
                                                           + "WHERE tipoTurno=? and UC_idUC=?");
            stm2.setString(1, tipoTurno);
            stm2.setString(2, idUC);
            ResultSet rs = stm2.executeQuery();
            while (rs.next()) {
                idTurno = rs.getInt("idTurno");
            }
            
            //Agora, determinado o turno, colocar na tabela Aluno_has_Turno este novo Turno.
            PreparedStatement stm3 = conn.prepareStatement("INSERT INTO Aluno_has_Turno\n"
                                                           + "VALUES (?, ?, ?)\n");
            stm3.setInt(1, idAluno);
            stm3.setInt(2, idTurno);
            stm3.setInt(3, 0);
            stm3.executeUpdate();
            
            PreparedStatement stm4 = conn.prepareStatement("UPDATE turno\n"
                    + "Set capacidadeAtual = capacidadeAtual + 1\n"
                    + "Where idTurno = ?");
            
            stm4.setInt(1,idTurno);
            stm4.executeUpdate();
        } catch (Exception e) {
            //e.printStackTrace();
            throw new MySQLIntegrityConstraintViolationException();
        } finally {
            Connect.close(conn);
        }
    }

    public static void unsubscribeUC (String idUCs, int idAluno, String tipoTurno) {
        Connection conn = null;
        int idTurno = -1;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("DELETE FROM UnidadeCurricular_Has_Aluno\n" +
                "WHERE UnidadeCurricular_idUC=? and Aluno_idAluno=?");
            stm.setString(1, idUCs);
            stm.setInt(2, idAluno);
            stm.executeUpdate();
            
            /* Remover da tabela Aluno_has_Turno a entrada de esse Aluno na tabela. */
            PreparedStatement stm2 = conn.prepareStatement("SELECT idTurno FROM Turno\n" +
                                                           "WHERE tipoTurno=? and UC_idUC=?");
            stm2.setString(1, tipoTurno);
            stm2.setString(2, idUCs);
            ResultSet rs = stm2.executeQuery();
            if (rs.next()) {
                idTurno = rs.getInt("idTurno");
            }
            
            PreparedStatement stm3 = conn.prepareStatement("DELETE FROM Aluno_has_Turno\n" +
                                                           "WHERE Aluno_idAluno=? and Turno_idTurno=?");
            stm3.setInt(1, idAluno);
            stm3.setInt(2, idTurno);
            stm3.executeUpdate();
            
            PreparedStatement stm4 = conn.prepareStatement("UPDATE turno\n"
                    + "Set capacidadeAtual = capacidadeAtual - 1\n"
                    + "Where idTurno = ?");
            
            stm4.setInt(1,idTurno);
            stm4.executeUpdate();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    } 
}
