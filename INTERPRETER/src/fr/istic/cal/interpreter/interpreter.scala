package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  // TODO TP2
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil => NlValue
      case (a,b)::reste => if(a==v) b else lookUp(v,reste)
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire, augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
   * modifiée pour prendre en compte la nouvelle valeur de v sinon
   */
  // TODO TP2
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    val assoc = (v,d)
    mem match {
      case Nil => assoc :: Nil
      case (a,b)::reste => if(a==v) (a,d) :: reste else (a,b) :: assign(v,d,reste)
    }   
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  // TODO TP2
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl ⇒ NlValue
      case Cst(s) ⇒ CstValue(s)
      case VarExp(s) ⇒ lookUp(Var(s),mem)
      case Cons(arg1, arg2) ⇒ ConsValue(interpreterExpr(arg1 ,mem), interpreterExpr(arg2 ,mem))
      case Hd(arg) ⇒ interpreterExpr(arg ,mem) match {
        case ConsValue(a,b) => a
        case _ => NlValue
      }
      case Tl(arg) ⇒ interpreterExpr(arg ,mem) match {
        case ConsValue(a,b) => b
        case _ => NlValue
      }
      case Eq(arg1, arg2) ⇒ interpreterExpr(arg1 ,mem)==interpreterExpr(arg2 ,mem) match {
        case true if(interpreterExpr(arg1 ,mem)==NlValue) => NlValue
        case true => null
        case false => NlValue
      }
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant une expression de cette valeur
   */
  // TODO TP2
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue ⇒ Nl
      case CstValue(s) ⇒ Cst(s)
      case ConsValue(s1 ,s2 ) ⇒ Cons(valueToExpression(s1) , valueToExpression(s2))
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  // TODO TP2
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop => memory
      case Set(v,e) => assign(v,interpreterExpr(e,memory),memory)
      case For(c,b) => interpreterCommands(b,memory)
      case If(c,ifc,elc) => memory
      case While(i,b) => memory
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  // TODO TP2
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = ???

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  // TODO TP2
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = ???

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = ???

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreter(program: Program, vals: List[Value]): List[Value] = ???

}