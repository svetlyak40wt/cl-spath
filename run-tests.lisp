(require 'heresy)
(push #p"./" asdf:*central-registry*)
(asdf:oos 'asdf:load-op 'spath)
(asdf:oos 'asdf:load-op 'spath-tests)

(sb-ext:quit)
