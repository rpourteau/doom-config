;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))
(global-set-key (kbd "C-z") 'undo)   ;; Bind C-Z to undo
(global-set-key (kbd "C-c d") #'lsp-find-definition) ;; Easier than to use the mouse to go to definition
(setq global-hl-line-modes nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "rpourteau"
      user-mail-address "remy.pourteau@cosylab.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Courier New" :height 110))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Highlight-symbol
(use-package! highlight-symbol
  :load-path "lisp/"
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))


(use-package! systemrdl-mode
  :mode ("\\.rdl\\'" . systemrdl-mode))

(use-package! vhdl-mode
    :mode ("\\.vhd\\'" "\\.vhdl\\'")
    :custom
    ;; Formatting
    (vhdl-basic-offset 3)
    (vhdl-align-groups t)
    (vhdl-inline-comment-column 40)
    (vhdl-end-comment-column 200)
    ;; Behavior
    (vhdl-clock-edge-condition 'function)
    (vhdl-electric-mode nil)
    (vhdl-reset-kind 'sync)
    (vhdl-self-insert-comments nil)
    (vhdl-stutter-mode t)
    ;; Custom templates
    (vhdl-model-alist
      '(("process general"
	 "<label> : process(<clock>)\12begin\12   if rising_edge(<clock>) then\12      <cursor>\12\12      if <reset> = '1' then\12         Reset;\12      end if;\12   end if;\12end process <label>;"
	 "p" "")
	("new document"
	 "---------------------------------------------------------------------------------------------------\12-- @brief <moduleName>\12-- @details\12--\12--\12-- @author Remy Pourteau, Cosylab (remy.pourteau@cosylab.com)\12-- @file <moduleName>.vhd\12--\12-- Copyright (c) 2020 Cosylab d.d.\12-- This software is distributed under the terms found\12-- in file LICENSE.txt that is included with this distribution.\12---------------------------------------------------------------------------------------------------\12library ieee;\12use ieee.std_logic_1164.all;\12use ieee.numeric_std.all;\12use ieee.math_real.all;\12\12-- CslLib packages\12use work.CslStdRtlPkg.all;\12---------------------------------------------------------------------------------------------------\12entity <moduleName> is\12   generic(\12      -- Add generic here\12   );\12   port(\12      -- Clock and Reset\12      clk_i      : in  sl;\12      rst_i      : in  sl\12      );\12end <moduleName>;\12---------------------------------------------------------------------------------------------------\12architecture rtl of <moduleName> is\12\12   -- Record containing all register elements\12   type RegType is record\12      CslTwoProcessTemplateSig  : sl;\12   end record RegType;\12\12   -- Initial and reset values for all register elements\12   constant REG_INIT_C: RegType :=(\12      CslTwoProcessTemplateSig    => '0');\12\12   -- Output of registers\12   signal r: RegType;\12\12   -- Combinational input to registers\12   signal rin: RegType;\12\12---------------------------------------------------------------------------------------------------\12begin\12   <cursor>\12\12\12end rtl;\12\12"
	 "n" "")
	("testBench"
	 "---------------------------------------------------------------------------------------------------\12-- @brief Test bench for simulating <UUT_Name> module behavior.\12--\12-- @author Remy Pourteau, Cosylab (remy.pourteau@cosylab.com)\12-- @file <UUT_Name>Tb.vhd\12--\12-- Copyright (c) 2020 Cosylab d.d.\12-- This software is distributed under the terms found\12-- in file LICENSE.txt that is included with this distribution.\12---------------------------------------------------------------------------------------------------\12library ieee;\12use ieee.std_logic_1164.all;\12use ieee.numeric_std.all;\12use ieee.math_real.all;\12use std.env.all;\12use work.CslStdRtlPkg.all;\12use work.CslTbAxiPkg.all;\12use work.CslTbBasePkg.all;\12use work.CslPrintfPkg.all;\12---------------------------------------------------------------------------------------------------\12\12entity <UUT_Name>Tb is\12end entity <UUT_Name>Tb;\12\12---------------------------------------------------------------------------------------------------\12\12architecture behavior of <UUT_Name>Tb is\12\12   -------------------------------------------------------------------------------------------------\12   -- Constant declarations\12   constant CLK_PERIOD_C     : time      := <clk_period_ns> ns;\12\12   -------------------------------------------------------------------------------------------------\12   -- UUT signals\12   signal clk_i       : sl                      := '0';\12   signal rst_i       : sl                      := '1';\12\12begin\12\12   -------------------------------------------------------------------------------------------------\12   -- Clocks\12   tbClock(clk_i, CLK_PERIOD_C);\12\12   -------------------------------------------------------------------------------------------------\12   -- UUT Instance\12   <cursor>\12\12   -------------------------------------------------------------------------------------------------\12   -- Testbench Stimulus\12   p_Sim : process\12   begin\12\12      printf(\"-----------------------------------------------------------------------------------\");\12      printf(\"Simulation start @ \\@\");\12      printf(\"Init \\@\");\12\12      tbReset(clk_i, rst_i, '1', 10);\12      tbClkPeriod(clk_i, 1);\12      printf(\"Reset done @ \\@\");\12\12      printf(\"-----------------------------------------------------------------------------------\");\12      printf(\"Checks that pass. @ \\@\");\12\12\12\12      tbClkPeriod(clk_i, 100);\12\12      tbReportErrorCnt;\12      printf(\"-----------------------------------------------------------------------------------\");\12      printf(\"Simulation Finished @ \\@\");\12      printf(\"-----------------------------------------------------------------------------------\");\12      finish;\12   end process p_sim;\12end architecture behavior;"
	 "t" "")
	("SeqProc"
      "-----------------------------------------------------------------------------\12-- Sequential process\12p_Seq : process(<clock>)\12begin\12   if rising_edge(<clock>) then\12      r <= rin;\12   end if;\12end process p_Seq;"
      "s" "")
	("CombProc"
	 "   -----------------------------------------------------------------------------\12   -- Combinational process\12   p_Comb : process(all)\12      variable v : RegType;\12   begin\12\12      -- Initialize v with current value of all registers\12      v := r;\12\12      <cursor>\12      -----------------------------------------------------------------------------\12      -- Synchronous reset\12      if (<reset> = '1') then\12         v := REG_INIT_C;\12      end if;\12\12      -----------------------------------------------------------------------------\12      -- Drive registers inputs\12      rin <= v;\12\12      -----------------------------------------------------------------------------\12      -- Drive module outputs\12\12\12   end process p_Comb;"
	 "c" "")
	("FsmComb"
	 "   -----------------------------------------------------------------------------\12   -- Combinational process\12   p_Comb : process (all)\12      variable v : RegType;\12   begin\12      -- Latch the current value\12      v := r;\12\12      -- State Machine\12      case r.state is\12\12         when IDLE_S =>\12            <cursor>\12            v.state := IDLE_S;\12\12\12         when others =>\12            v := REG_INIT_C;\12\12      end case;\12\12      -----------------------------------------------------------------------------\12      -- Synchronous reset\12      if (rst_i = '1') then\12         v := REG_INIT_C;\12      end if;\12\12      -----------------------------------------------------------------------------\12      -- Drive registers inputs\12      rin <= v;\12\12      -----------------------------------------------------------------------------\12      -- Drive module outputs\12      output_o <= r.reg1;\12\12   end process p_Comb;"
	 "f" "")
	("package"
	 "---------------------------------------------------------------------------------------------------\12-- @brief Package with constants for <packageName>.vhd\12-- @details\12--\12--\12-- @author Remy Pourteau, Cosylab (remy.pourteau@cosylab.com)\12-- @file <packageName>.vhd\12--\12-- Copyright (c) 2020 Cosylab d.d.\12-- This software is distributed under the terms found\12-- in file LICENSE.txt that is included with this distribution.\12---------------------------------------------------------------------------------------------------\12\12library ieee;\12use ieee.std_logic_1164.all;\12use ieee.numeric_std.all;\12\12use work.CslStdRtlPkg.all;\12\12package <packageName> is\12   -----------------------------------------------------------------------------\12   -- Functions definition, description in body\12   -----------------------------------------------------------------------------\12   <cursor>\12\12\12   -----------------------------------------------------------------------------\12   -- Constants definition\12   -----------------------------------------------------------------------------\12\12   -----------------------------------------------------------------------------\12   -- Types definition\12   -----------------------------------------------------------------------------\12\12\12   -----------------------------------------------------------------------------\12   -- Types reset value constants\12   -----------------------------------------------------------------------------\12\12end <packageName>;\12\12package body <packageName> is\12end <packageName>;\12"
	 "k" "")
	("std_logic_vector" "slv(<msb> downto <lsb>)" "v" "")))
    )

;; Add Git and icons to the modeline
(after! doom-modeline
  (setq
   doom-modeline-github t
   doom-modeline-major-mode-icon t))

(use-package! vhdl-ext
  :init
  ;; Only enable LSP and navigation for now
  (setq vhdl-ext-feature-list
        '(font-lock       ;; Keep syntax highlight from vhdl-mode
          xref            ;; jump to definition
          lsp             ;; Enable LSP
          navigation      ;; Allow go-to-definition etc.
          imenu))         ;; Index for outline/sidebar
  :hook (vhdl-mode . vhdl-ext-mode)
  :config
  (vhdl-ext-mode-setup)
  (vhdl-ext-lsp-set-server 've-rust-hdl)) ;; Use vhdl_ls (Rust HDL)

(after! lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 2000)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "vhdl_ls")
    :major-modes '(vhdl-mode)
    :server-id 've-rust-hdl)))

(add-hook! vhdl-mode #'lsp!)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
