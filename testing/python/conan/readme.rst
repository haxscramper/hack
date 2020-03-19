This is a wrapper around conan package management system.

This wrapper allows you to write package definitions for conan build
as declarative toml files - instead of python code that can be parsed
only using python interpreter. `conan_toml.py` defined new base class
for package configurations - to use it in your package you will need
to import `conan_toml.py` and then write this in your `conanfile.py`:

.. code-block:: python
    class ConanTomlExample(common.TomlConfPackage):
        pass

All necessary configuration will be taken from `conffile.toml` (by
default). Or, if you want to customise file name you can declare
`conffile` attribute of the derived class and set it to the desired
file name.

Reading of the configuration file happends in the `__init__` of
`TomlConfPackage` - in case you want to customize some of the
variables afterwards.

TODO:

- [ ] Wrap `conan_toml.py` into conan package to use with
  `python_requires`.
