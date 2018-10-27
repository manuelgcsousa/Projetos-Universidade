namespace CleanicaFinal.Models
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    public partial class Funcionario_ServicosRealizados
    {
        [Key]
        [Column(Order = 0)]
        [StringLength(50)]
        public string Funcionario_Utilizador_email { get; set; }

        [Key]
        [Column(Order = 1)]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public int ServicosRealizados_id_ServicosRealizados { get; set; }
    }
}
