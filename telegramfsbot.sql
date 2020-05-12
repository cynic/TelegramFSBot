--
-- PostgreSQL database dump
--

-- Dumped from database version 12.2
-- Dumped by pg_dump version 12.2

-- Started on 2020-05-08 06:42:05

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 2840 (class 1262 OID 16399)
-- Name: TelegramFSBot; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE "TelegramFSBot" WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'English_South Africa.1252' LC_CTYPE = 'English_South Africa.1252';


ALTER DATABASE "TelegramFSBot" OWNER TO postgres;

\connect "TelegramFSBot"

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 204 (class 1259 OID 16412)
-- Name: questions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.questions (
    id integer NOT NULL,
    question text NOT NULL,
    answer text NOT NULL,
    interpretation smallint NOT NULL,
    submitter bigint,
    "nInputs" smallint DEFAULT 0 NOT NULL
);


ALTER TABLE public.questions OWNER TO postgres;

--
-- TOC entry 203 (class 1259 OID 16410)
-- Name: questions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.questions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.questions_id_seq OWNER TO postgres;

--
-- TOC entry 2841 (class 0 OID 0)
-- Dependencies: 203
-- Name: questions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.questions_id_seq OWNED BY public.questions.id;


--
-- TOC entry 205 (class 1259 OID 16427)
-- Name: seen; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.seen (
    "user" bigint NOT NULL,
    question integer NOT NULL
);


ALTER TABLE public.seen OWNER TO postgres;

--
-- TOC entry 202 (class 1259 OID 16405)
-- Name: users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.users (
    userid bigint NOT NULL,
    mode smallint NOT NULL,
    question integer
);


ALTER TABLE public.users OWNER TO postgres;

--
-- TOC entry 2696 (class 2604 OID 16415)
-- Name: questions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.questions ALTER COLUMN id SET DEFAULT nextval('public.questions_id_seq'::regclass);


--
-- TOC entry 2701 (class 2606 OID 16420)
-- Name: questions questions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.questions
    ADD CONSTRAINT questions_pkey PRIMARY KEY (id);


--
-- TOC entry 2705 (class 2606 OID 16431)
-- Name: seen seen_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.seen
    ADD CONSTRAINT seen_pkey PRIMARY KEY ("user", question);


--
-- TOC entry 2699 (class 2606 OID 16409)
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (userid);


--
-- TOC entry 2697 (class 1259 OID 16426)
-- Name: fki_question_fk; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_question_fk ON public.users USING btree (question);


--
-- TOC entry 2702 (class 1259 OID 16443)
-- Name: fki_questions_fk; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_questions_fk ON public.seen USING btree (question);


--
-- TOC entry 2703 (class 1259 OID 16437)
-- Name: fki_user_fk; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_user_fk ON public.seen USING btree ("user");


--
-- TOC entry 2706 (class 2606 OID 16421)
-- Name: users question_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT question_fk FOREIGN KEY (question) REFERENCES public.questions(id) NOT VALID;


--
-- TOC entry 2708 (class 2606 OID 16438)
-- Name: seen questions_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.seen
    ADD CONSTRAINT questions_fk FOREIGN KEY (question) REFERENCES public.questions(id) NOT VALID;


--
-- TOC entry 2707 (class 2606 OID 16432)
-- Name: seen user_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.seen
    ADD CONSTRAINT user_fk FOREIGN KEY ("user") REFERENCES public.users(userid) NOT VALID;


-- Completed on 2020-05-08 06:42:05

--
-- PostgreSQL database dump complete
--

