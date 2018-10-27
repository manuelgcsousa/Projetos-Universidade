package data;

import business.UtilizadorInexistenteException;
import business.Docente;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class DocenteDAO {

    public static Docente get (int idDocente) throws UtilizadorInexistenteException {
        Docente d = null;
        Connection conn = null;
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM Docente WHERE idDocente=?");
            stm.setInt(1, idDocente);
            ResultSet rs = stm.executeQuery();
            if (rs.next()) {
                String user = rs.getString("username");
                String pass = rs.getString("password");
                String nome = rs.getString("nome");
                String idUC = rs.getString("idUC");
                d = new Docente(user, pass, nome, idUC);
            } else {
                throw new UtilizadorInexistenteException("Utilizador Inexistente!");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return d;
    }
}
