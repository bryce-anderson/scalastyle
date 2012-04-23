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

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.scalastyle.ScalariformChecker
import org.scalastyle._

class CyclomaticComplexityChecker extends AbstractMethodChecker {
  import VisitorHelper._
  val errorKey = "cyclomatic.complexity"
  val DefaultMaximum = 10
  private lazy val maximum = getInt("maximum", DefaultMaximum)
  private val tokens = Set(IF, CASE, WHILE, DO, FOR)

  override def params() = List[String]("" + maximum)

  def matches(t: BaseClazz[AstNode]): Boolean = {
    matchFunDefOrDcl(t, cyclomaticComplexity(maximum) _)
  }

  private def isLogicalOrAnd(t: Token) = t.tokenType == VARID && (t.text == "&&" || t.text == "||")

  private def cyclomaticComplexity(maximum: Int)(t: FunDefOrDcl): Boolean = {
    t.tokens.count(t => tokens.contains(t.tokenType) || isLogicalOrAnd(t)) + 1 > maximum
  }
}