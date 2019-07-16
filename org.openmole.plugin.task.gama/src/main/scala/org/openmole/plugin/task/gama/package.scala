/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.task

import org.openmole.core.workflow.builder._
import org.openmole.core.workflow.dsl._
import org.openmole.core.context._
import org.openmole.core.expansion._
import org.openmole.plugin.task.gama.GamaTask._

package gama {

  trait GamaPackage {
    lazy val gamaInputs = new {
      def +=[T: GAMABuilder: InputOutputBuilder](p: Val[_]*): T => T = p.map(p => this.+=[T](p, p.name))
      def +=[T: GAMABuilder: InputOutputBuilder](p: Val[_], name: String): T => T = (implicitly[GAMABuilder[T]].gamaInputs add (FromContext.prototype(p) -> name)) andThen (inputs += p)
      def +=[T: GAMABuilder: InputOutputBuilder, U](v: U, name: String)(implicit toFromContext: ToFromContext[U, U]): T => T = implicitly[GAMABuilder[T]].gamaInputs add (toFromContext(v) -> name)
    }

    lazy val gamaOutputs = new {
      def +=[T: GAMABuilder: InputOutputBuilder](p: Val[_]*): T => T = p.map(p => this.+=[T](p.name, p))
      def +=[T: GAMABuilder: InputOutputBuilder](name: String, p: Val[_]): T => T = (implicitly[GAMABuilder[T]].gamaOutputs add name -> p) andThen (outputs += p)
    }

    lazy val gamaSeed = new {
      def :=[T: GAMABuilder](seed: Val[Int]): T => T = implicitly[GAMABuilder[T]].seed.set(Some(seed))
    }
  }

}

package object gama extends GamaPackage
