Plutus Tools SDK User Guide
===============================

Intended Audience
-------------------------------

This user guide is intended for external developers who are authoring distributed 
applications that implement smart contracts on the Cardano blockchain. 

Plutus Tools SDK
===============================

The Plutus Tools SDK is a collection of off-chain infrastructure resources built 
for external developers. The `Plutus Tools SDK repository <https://github.com/
input-output-hk/plutus-apps>`_ supports the underlying resources that developers 
need who are writing full applications using Plutus in Haskell, including off-chain 
code. The term “off-chain code” refers to the part of a contract application’s code 
which runs off the chain. 

The `Plutus Tools SDK repository <https://github.com/input-output-hk/plutus-apps>`_ 
contains packages such as: 

* the `Plutus Application Backend (PAB) <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab>`_, 
  an off-chain application for managing the state of Plutus contract instances;

* the `chain-index <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index>`_, 
  a lightweight, customizable chain follower application and library for DApp 
  developers who need to index and query the Cardano blockchain; 

* a variety of other Plutus packages. 

To get started using the Plutus Tools SDK, see the `Plutus starter template repository 
<https://github.com/input-output-hk/plutus-starter>`_.

.. toctree::
   :caption: Explore Plutus
   :maxdepth: 2

   plutus/explanations/index
   plutus/tutorials/index
   plutus/howtos/index
   plutus/troubleshooting

.. toctree::
   :caption: Architecture design records
   :maxdepth: 1

   adr/index

.. toctree::
   :caption: Reference
   :maxdepth: 2

   reference/index
