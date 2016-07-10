// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import _root_.scalariform.lexer.Tokens.LBRACKET
import _root_.scalariform.parser._

class SortedImportsChecker extends ScalariformChecker {
  // val errorKey = "no.whitespace.before.left.bracket"
  val errorKey = "import.sorted"

  private case class Import(offset: Int, text: String)

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val CompilationUnit(stats, _) = ast
    val imports = stats.immediateChildren.flatMap { line =>
      line match {
        case ImportClause(_, expr, _) =>
          Some(Import(line.firstToken.offset, expr.tokens.map(_.text).mkString))

        case _ =>
          Nil
      }
    }

    checkImports(imports)
  }

  private def checkImports(imports: List[Import]): List[ScalastyleError] = imports match {
    case a::(rst@ b::xs) if b.text < a.text => newError(b)::checkImports(rst)
    case _::(rst@ b::xs) => checkImports(rst)
    case _ => Nil
  }

  private def newError(badImport: Import, args: Any*): ScalastyleError =
    PositionError(
      badImport.offset,
      args.map(_.toString).toList,
      Some(this.errorKey + ": import " + badImport.text)
    )
}
