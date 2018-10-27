package data;


import business.TurnoInexistenteException;
import business.Turno;
import business.TurnoCheioException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class TurnoDAO {
    
    public static Turno get (int idTurno, String idUC) throws TurnoInexistenteException {
        Map<Integer, Integer> faltas = new HashMap<Integer, Integer>();
        Turno tu = null;
        Connection conn = null;
        String tipoTurno = null;
        String sala = null;
        int capacidadeAtual = 0;
        int capacidadeMax = 0;
        int idAluno = 0;
        int falta = 0;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM Turno WHERE id=?,UC_idUC=?");
            stm.setInt(1, idTurno);
            stm.setString(2, idUC);
            ResultSet rs = stm.executeQuery();
            
            if (rs.next()) {
                tipoTurno = rs.getString("tipoTurno");
                sala = rs.getString("sala");
                capacidadeAtual = rs.getInt("capacidadeAtual");
                capacidadeMax = rs.getInt("capacidadeMax");
            } else {
                throw new TurnoInexistenteException("Utilizador Inexistente!");
            }
            
            stm = conn.prepareStatement("SELECT * FROM Aluno_Has_Turno WHERE Turno_idTurno = ?");
            stm.setInt(1, idTurno);
            ResultSet r = stm.executeQuery();
            
            while (r.next()) {
                idAluno = rs.getInt("Aluno_idAluno");
                falta = rs.getInt("faltas");
                faltas.put(idAluno, falta);
            }
            
            tu = new Turno(idTurno, tipoTurno, sala, capacidadeAtual, capacidadeMax, idUC, faltas);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return tu;
    }
    
    public static void put (Turno turno) {
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("INSERT INTO Turno\n" +
                "VALUES (tipoTurno, sala, capacidadeAtual, capacidadeMax, UC_idUC) (?, ?, ?, ?, ?)\n");
            stm.setString(1, turno.getTipo());
            stm.setString(2, turno.getSala());
            stm.setInt(3, 0);
            stm.setInt(4, 30);
            stm.setString(5, turno.getidUC());
            stm.executeQuery(); 
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    public static ArrayList<String> getTurnosUC (String idUC) {
        Connection conn = null;
        String tipoTurno;
        ArrayList<String> tipoTurnos = new ArrayList<String>();
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT DISTINCT tipoTurno\n" +
                                                          "FROM Turno\n" +
                                                          "WHERE UC_idUC=?");
            stm.setString(1, idUC);
            ResultSet rs = stm.executeQuery();
            
            while (rs.next()) {
                tipoTurno = rs.getString("tipoTurno");
                tipoTurnos.add(tipoTurno);
            }
        } catch (Exception e) {
            //fazer cenas
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        
        return tipoTurnos;
    }
    
    public static Map<Integer, String[]> getFaltasAlunos (String tipoTurno, String idUC) {
        Connection conn = null;
        int idTurno = 0;
        Map<Integer, String[]> faltasTurno = new HashMap<Integer, String[]>();
        
        try {
            conn = Connect.connect();
            
            idTurno = getidTurno_capTurno(idUC, tipoTurno)[0];
            
            PreparedStatement stm2 = conn.prepareStatement("SELECT Aluno_idAluno, faltas, nome\n" +
                                                           "FROM Aluno_has_Turno\n" +
                                                           "JOIN Aluno ON Aluno_idAluno = idAluno\n" +
                                                           "WHERE Turno_idTurno = ?");
            stm2.setInt(1, idTurno);
            ResultSet r = stm2.executeQuery();
            
            while (r.next()) {
                String[] info = new String[2];
                int idAluno = r.getInt("Aluno_idAluno");
                int faltas = r.getInt("faltas");
                String nome = r.getString("nome");
                info[0] = nome;
                info[1] = faltas + "";
                faltasTurno.put(idAluno, info);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        
        return faltasTurno;
    }

    public static void addfalta (int idAluno, String idUC, String tipoTurno,int falta) {
        Connection conn = null;
        try {
            conn = Connect.connect();
            
            int idTurno = getidTurno_capTurno(idUC, tipoTurno)[0];
            
            
            PreparedStatement stm = conn.prepareStatement("UPDATE Aluno_has_Turno\n" +
            "SET faltas = ?\n" + 
                        "WHERE Aluno_idAluno = ? and Turno_idTurno = ?");
                
            stm.setInt(1, falta);
            stm.setInt(2, idAluno);
            stm.setInt(3, idTurno);
            stm.executeUpdate(); 
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    public static int[] getidTurno_capTurno (String idUC, String tipoTurno) {
        Connection conn = null;
        int[] info = new int[3];
        int idTurno = -1;
        int cap = 0;
        int capAtual = 0;
        try {
            conn = Connect.connect();
            
            PreparedStatement st = conn.prepareStatement("SELECT idTurno, capacidadeMax, capacidadeAtual\n" +
                                                         "FROM Turno WHERE tipoTurno = ? and UC_idUC = ?");
            st.setString(1, tipoTurno);
            st.setString(2, idUC);
            ResultSet rs = st.executeQuery();
            
            if (rs.next()) {
                idTurno = rs.getInt("idTurno");
                cap = rs.getInt("capacidadeMax");
                capAtual = rs.getInt("capacidadeAtual");
                info[0] = idTurno;
                info[1] = cap;
                info[2] = capAtual;
            }           
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        
        return info;
    }

    public static void alteraCpcd (String idUC, String tipoTurno, Integer newCapacidad) {
        Connection conn = null;
       
        try {
            conn = Connect.connect();
           
            int idTurno = getidTurno_capTurno(idUC, tipoTurno)[0];
           
            PreparedStatement stm = conn.prepareStatement("UPDATE Turno\n" +
                                                "SET capacidadeMax = ?\n" + 
                                                "WHERE idTurno = ?");
            stm.setInt(1, newCapacidad);
            stm.setInt(2, idTurno);
            stm.executeUpdate();   
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    /**
     *
     * @param idUC
     * @param tipoTurno
     * @param faltas
     * @param idTurno
     * @return
     */
    public static ArrayList<Integer> getAlunosFaltosos (String idUC, String tipoTurno) {
        Connection conn = null;
        ArrayList<Integer> alunosFaltosos = new ArrayList<Integer>();

        try {
            conn = Connect.connect();
            
            int idTurno = TurnoDAO.getidTurno_capTurno(idUC, tipoTurno)[0];
            int faltas = UnidadeCurricularDAO.getFaltasUC(idUC);
            
            PreparedStatement st = conn.prepareStatement("SELECT Aluno_idAluno\n" +
                                                         "FROM Aluno_has_Turno WHERE Turno_idTurno = ? and faltas >= ?");
            st.setInt(1, idTurno);
            st.setInt(2, faltas);
            ResultSet rs = st.executeQuery();
            
            if (rs.next()) {
                int idAluno = rs.getInt("Aluno_idAluno");
                alunosFaltosos.add(idAluno);
            }           
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        
        return alunosFaltosos;
    }

    public static int getTurnoAntigo (String idUC, int idAluno) {
        Connection conn = null;
        int idTurnoAntigo = -1;
        
        try {
            conn = Connect.connect();
            
            PreparedStatement st = conn.prepareStatement("SELECT idTurno\n" +
                                                         "FROM aluno_has_turno join Turno on Turno_idTurno = idTurno " +
                                                         "WHERE UC_idUC = ? and Aluno_idAluno = ?");
            st.setString(1, idUC);
            st.setInt(2, idAluno);
            ResultSet rs = st.executeQuery();
            
            if (rs.next()) {
                idTurnoAntigo = rs.getInt("idTurno");
            }           
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        
        return idTurnoAntigo;
    }
    
    public static void insereAlunoEstatuto (int idAluno, String idUC, 
                                            String turnoPretendido)
                                                throws TurnoCheioException
    {
        Connection conn = null;
        int idTurno = -1, capacidadeAtual = -1, capacidadeMax = -1;
        
        try {
            conn = Connect.connect();
            
            String SQL = "SELECT idTurno, capacidadeAtual, capacidadeMax\n" +
                         "FROM Turno WHERE tipoTurno = ? and UC_idUC = ?";
            PreparedStatement stm = conn.prepareStatement(SQL);
            stm.setString(1, turnoPretendido);
            stm.setString(2, idUC);
            ResultSet rs = stm.executeQuery();
            
            if (rs.next()) {
                idTurno = rs.getInt("idTurno");
                capacidadeAtual = rs.getInt("capacidadeAtual");
                capacidadeMax = rs.getInt("capacidadeMax");
            }
            
            /* Verifica capacidade do Turno.
               Se for suficiente, põe Aluno no Turno pretendido. */
            if (capacidadeAtual + 1 <= capacidadeMax) {
                String SQL_UpdateTurno = "UPDATE Aluno_has_Turno\n" + 
                                         "SET Turno_idTurno = ?\n" +
                                         "WHERE Aluno_idAluno = ?";
                PreparedStatement stm2 = conn.prepareStatement(SQL_UpdateTurno);
                stm2.setInt(1, idTurno);
                stm2.setInt(2, idAluno);
                stm2.executeUpdate();
                
                /* Atualiza capacidadeAtual do Turno. */
                String SQL_UpdateCapacidade = "UPDATE Turno\n" +
                                              "SET capacidadeAtual = ?\n" +
                                              "WHERE idTurno = ?";
                PreparedStatement stm3 = conn.prepareStatement(SQL_UpdateCapacidade);
                stm3.setInt(1, capacidadeAtual + 1);
                stm3.setInt(2, idTurno);
            } else {
                throw new TurnoCheioException();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
}