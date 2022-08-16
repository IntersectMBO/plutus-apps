Plutus Tools SDK User Guide
===============================

Intended Audience
-------------------------------

This user guide is intended for developers who are authoring distributed applications 
("DApps") by using smart contracts on the Cardano blockchain. 

.. note::
   If you are a developer who wants to contribute to the Plutus Tools SDK project, 
   please refer to documentation residing in the `Plutus Tools SDK repository <https://github.com/
   input-output-hk/plutus-apps>`_.

Plutus Tools SDK
===============================

The Plutus Tools SDK is a collection of off-chain infrastructure resources built 
for external developers. The `Plutus Tools SDK repository <https://github.com/
input-output-hk/plutus-apps>`_ supports the underlying resources that developers 
need who are writing full applications using Plutus in Haskell, including off-chain 
code. 

The term “off-chain code” refers to the part of a contract application’s code 
which runs outside of the blockchain. Off-chain code responds to events happening 
on the blockchain, usually by producing transactions. 

The `Plutus Tools SDK repository <https://github.com/input-output-hk/plutus-apps>`_ 
contains packages such as: 

* the `Plutus Application Backend (PAB) <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab>`_, 
  an off-chain application for managing the state of Plutus contract instances;

* the `chain-index <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index>`_, 
  a lightweight, customizable chain follower application and library for DApp 
  developers who need to index and query the Cardano blockchain; 

* a variety of other Plutus packages. 

Plutus Starter Template Repository
----------------------------------------

See the `Plutus starter template repository <https://github.com/input-output-hk/plutus-starter>`_ 
for a simple starter project using the Plutus Tools SDK. 

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
